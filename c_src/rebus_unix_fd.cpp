// Native handles for Unix file descriptors, behind Rebus.UnixFD.
//
// A handle is a resource that owns one descriptor. It is logically open until
// an explicit close, the exit of its owning process, or garbage collection,
// and whichever comes first is the only path that reaches close(2). Once a
// handle is closed it forgets the descriptor number, so a stale handle can
// never act on a number the operating system has since handed out again.
//
// A pin keeps the descriptor itself open while the caller uses the raw
// number. Closing a pinned handle marks it closed at once but defers close(2)
// to the last unpin. Each pin is a token resource of its own, held by the
// pinning process, so a pin is released even if that process is killed: its
// heap, and with it the last reference to the token, goes away.
//
// Fine (0.1) registers resource types without a down callback, so the handle
// type is registered by hand in the load callback; everything else goes
// through Fine.

#include <erl_nif.h>
#include <fine.hpp>

#include <atomic>
#include <cerrno>
#include <climits>
#include <cstdint>
#include <mutex>
#include <new>
#include <stdexcept>
#include <tuple>
#include <variant>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

namespace {

namespace atoms {
auto closed = fine::Atom("closed");
auto ebadf = fine::Atom("ebadf");
auto einval = fine::Atom("einval");
auto emfile = fine::Atom("emfile");
auto enfile = fine::Atom("enfile");
auto enomem = fine::Atom("enomem");
auto eunknown = fine::Atom("eunknown");
auto invalid_descriptor = fine::Atom("invalid_descriptor");
auto noproc = fine::Atom("noproc");
auto not_local = fine::Atom("not_local");
auto not_owner = fine::Atom("not_owner");
auto ok = fine::Atom("ok");
} // namespace atoms

fine::Atom errno_atom(int error) {
  switch (error) {
  case EBADF:
    return atoms::ebadf;
  case EINVAL:
    return atoms::einval;
  case EMFILE:
    return atoms::emfile;
  case ENFILE:
    return atoms::enfile;
  case ENOMEM:
    return atoms::enomem;
  default:
    return atoms::eunknown;
  }
}

// Handles currently allocated, for the test hooks.
std::atomic<std::int64_t> live_handles{0};

ErlNifResourceType *handle_type = nullptr;

struct Handle {
  explicit Handle(int descriptor) : fd(descriptor) { ++live_handles; }
  ~Handle() { --live_handles; }

  Handle(const Handle &) = delete;
  Handle &operator=(const Handle &) = delete;

  std::mutex mutex;
  // The descriptor, or -1 once close(2) has run.
  int fd;
  // False once the handle is closed, even while pins defer close(2).
  bool open = true;
  unsigned pins = 0;
  ErlNifPid owner{};
  ErlNifMonitor monitor{};
  bool monitored = false;
  // How many times close(2) ran for this handle; never more than one.
  std::atomic<std::uint64_t> closes{0};
};

// Marks a handle closed and returns the descriptor the caller must pass to
// release_fd/2 once the mutex is dropped, or -1 if close(2) is not due yet
// (the handle is pinned) or has already run. The caller holds the mutex.
int retire_locked(Handle &handle) {
  handle.open = false;

  if (handle.pins > 0 || handle.fd < 0) {
    return -1;
  }

  int fd = handle.fd;
  handle.fd = -1;
  return fd;
}

// Closes a descriptor taken by retire_locked/1 or the last unpin. close(2)
// releases the number even when it reports an error (including EINTR on
// Linux and macOS), so it is never retried.
void release_fd(Handle &handle, int fd) {
  if (fd < 0) {
    return;
  }

  ::close(fd);
  ++handle.closes;
}

// Drops the owner monitor, if any. The caller holds the mutex.
void demonitor_locked(ErlNifEnv *env, Handle &handle) {
  if (handle.monitored) {
    // A non-zero result means the monitor has already fired; its down
    // callback finds `monitored` false and does nothing.
    enif_demonitor_process(env, &handle, &handle.monitor);
    handle.monitored = false;
  }
}

void handle_dtor(ErlNifEnv *, void *object) {
  auto *handle = static_cast<Handle *>(object);

  // Nothing else can reach the handle now, and pins hold a reference, so the
  // descriptor is still set only if no other path closed it.
  if (handle->fd >= 0) {
    ::close(handle->fd);
    handle->fd = -1;
  }

  handle->~Handle();
}

void handle_down(ErlNifEnv *, void *object, ErlNifPid *, ErlNifMonitor *mon) {
  auto *handle = static_cast<Handle *>(object);
  int fd;

  {
    std::lock_guard<std::mutex> lock(handle->mutex);

    // A monitor replaced by controlling_process/2 can still fire; only the
    // current owner's exit closes the handle.
    if (!handle->monitored ||
        enif_compare_monitors(mon, &handle->monitor) != 0) {
      return;
    }

    handle->monitored = false;
    fd = retire_locked(*handle);
  }

  release_fd(*handle, fd);
}

Handle *get_handle(ErlNifEnv *env, fine::Term term) {
  void *object;

  // A handle that crossed a distribution boundary decodes as a plain
  // reference, and so does any reference that is not a handle.
  if (!enif_get_resource(env, term, handle_type, &object)) {
    return nullptr;
  }

  return static_cast<Handle *>(object);
}

using HandleResult = std::variant<fine::Ok<fine::Term>, fine::Error<fine::Atom>>;

// Wraps an open descriptor in a new handle owned by `owner`. The descriptor
// is consumed: if the owner is not alive it is closed at once.
HandleResult make_handle(ErlNifEnv *env, int fd, const ErlNifPid &owner) {
  void *memory = enif_alloc_resource(handle_type, sizeof(Handle));
  if (memory == nullptr) {
    ::close(fd);
    return fine::Error(atoms::enomem);
  }

  auto *handle = new (memory) Handle(fd);
  bool alive;
  int dead_fd = -1;

  {
    // Hold the mutex across the monitor so that an owner which exits at once
    // cannot run the down callback before `monitored` is set.
    std::lock_guard<std::mutex> lock(handle->mutex);
    handle->owner = owner;
    alive = enif_monitor_process(env, handle, &owner, &handle->monitor) == 0;

    if (alive) {
      handle->monitored = true;
    } else {
      dead_fd = retire_locked(*handle);
    }
  }

  release_fd(*handle, dead_fd);

  if (!alive) {
    enif_release_resource(memory);
    return fine::Error(atoms::noproc);
  }

  auto term = fine::Term(enif_make_resource(env, memory));
  enif_release_resource(memory);
  return fine::Ok(term);
}

// A pin on a handle, released when unpinned explicitly or when the token is
// garbage collected, whichever happens first.
struct PinToken {
  explicit PinToken(Handle *pinned) : handle(pinned) {
    enif_keep_resource(handle);
  }

  PinToken(const PinToken &) = delete;
  PinToken &operator=(const PinToken &) = delete;

  void destructor(ErlNifEnv *) {
    release();
    enif_release_resource(handle);
  }

  void release() {
    if (released.exchange(true)) {
      return;
    }

    int fd = -1;

    {
      std::lock_guard<std::mutex> lock(handle->mutex);
      --handle->pins;

      if (handle->pins == 0 && !handle->open && handle->fd >= 0) {
        fd = handle->fd;
        handle->fd = -1;
      }
    }

    release_fd(*handle, fd);
  }

  Handle *handle;
  // Starts released, so a token that never took its pin gives nothing back.
  std::atomic<bool> released{true};
};

FINE_RESOURCE(PinToken);

auto load_registration = fine::Registration::register_load(
    [](ErlNifEnv *env, void **, fine::Term) {
      ErlNifResourceTypeInit init{};
      init.dtor = handle_dtor;
      init.down = handle_down;

      handle_type = enif_open_resource_type_x(env, "Rebus.UnixFD.Handle",
                                              &init, ERL_NIF_RT_CREATE,
                                              nullptr);

      if (handle_type == nullptr) {
        throw std::runtime_error("cannot open the Rebus.UnixFD.Handle type");
      }
    });

} // namespace

// Wraps a raw descriptor owned by the caller in a handle owned by `owner`.
HandleResult wrap(ErlNifEnv *env, std::int64_t fd, fine::Term owner_term) {
  if (fd < 0 || fd > INT_MAX) {
    return fine::Error(atoms::invalid_descriptor);
  }

  int descriptor = static_cast<int>(fd);

  // A number that is not open now would be closed later, after the operating
  // system may have reused it, so refuse it rather than adopt it. F_GETFD
  // fails only with EBADF; should it fail otherwise, the descriptor is open
  // and the caller has handed it over, so it is closed rather than leaked.
  if (fcntl(descriptor, F_GETFD) < 0) {
    int error = errno;

    if (error != EBADF) {
      ::close(descriptor);
    }

    return fine::Error(errno_atom(error));
  }

  ErlNifPid owner;
  if (!enif_get_local_pid(env, owner_term, &owner)) {
    ::close(descriptor);
    return fine::Error(atoms::not_local);
  }

  return make_handle(env, descriptor, owner);
}

FINE_NIF(wrap, 0);

std::variant<fine::Ok<>, fine::Error<fine::Atom>>
close_handle(ErlNifEnv *env, fine::Term term) {
  Handle *handle = get_handle(env, term);
  if (handle == nullptr) {
    return fine::Error(atoms::not_local);
  }

  int fd;

  {
    std::lock_guard<std::mutex> lock(handle->mutex);

    if (!handle->open) {
      return fine::Error(atoms::closed);
    }

    demonitor_locked(env, *handle);
    fd = retire_locked(*handle);
  }

  release_fd(*handle, fd);
  return fine::Ok();
}

FINE_NIF(close_handle, 0);

std::variant<fine::Ok<>, fine::Error<fine::Atom>>
controlling_process(ErlNifEnv *env, fine::Term term, fine::Term pid_term) {
  Handle *handle = get_handle(env, term);
  if (handle == nullptr) {
    return fine::Error(atoms::not_local);
  }

  ErlNifPid target;
  if (!enif_get_local_pid(env, pid_term, &target)) {
    return fine::Error(atoms::not_local);
  }

  ErlNifPid self;
  enif_self(env, &self);

  int fd;

  {
    std::lock_guard<std::mutex> lock(handle->mutex);

    if (!handle->open) {
      return fine::Error(atoms::closed);
    }

    if (enif_compare_pids(&self, &handle->owner) != 0) {
      return fine::Error(atoms::not_owner);
    }

    if (enif_compare_pids(&target, &handle->owner) == 0) {
      return fine::Ok();
    }

    ErlNifMonitor monitor;
    bool alive = enif_monitor_process(env, handle, &target, &monitor) == 0;

    demonitor_locked(env, *handle);

    if (alive) {
      handle->owner = target;
      handle->monitor = monitor;
      handle->monitored = true;
      return fine::Ok();
    }

    fd = retire_locked(*handle);
  }

  release_fd(*handle, fd);
  return fine::Error(atoms::noproc);
}

FINE_NIF(controlling_process, 0);

std::variant<fine::Ok<ErlNifPid>, fine::Error<fine::Atom>>
owner(ErlNifEnv *env, fine::Term term) {
  Handle *handle = get_handle(env, term);
  if (handle == nullptr) {
    return fine::Error(atoms::not_local);
  }

  std::lock_guard<std::mutex> lock(handle->mutex);

  if (!handle->open) {
    return fine::Error(atoms::closed);
  }

  return fine::Ok(handle->owner);
}

FINE_NIF(owner, 0);

std::variant<fine::Ok<fine::ResourcePtr<PinToken>, std::int64_t>,
             fine::Error<fine::Atom>>
pin(ErlNifEnv *env, fine::Term term) {
  Handle *handle = get_handle(env, term);
  if (handle == nullptr) {
    return fine::Error(atoms::not_local);
  }

  // Allocate the token first, so that taking the pin cannot fail halfway.
  auto token = fine::make_resource<PinToken>(handle);
  std::int64_t fd;

  {
    std::lock_guard<std::mutex> lock(handle->mutex);

    if (!handle->open) {
      return fine::Error(atoms::closed);
    }

    ++handle->pins;
    token->released = false;
    fd = handle->fd;
  }

  return fine::Ok(token, fd);
}

FINE_NIF(pin, 0);

// Releases a pin. Anything that is not a pin token, such as a token that
// crossed a distribution boundary, has no pin to release here.
fine::Atom unpin(ErlNifEnv *env, fine::Term term) {
  try {
    fine::decode<fine::ResourcePtr<PinToken>>(env, term)->release();
  } catch (const std::invalid_argument &) {
    // Not a pin token: nothing to release.
  }

  return atoms::ok;
}

FINE_NIF(unpin, 0);

HandleResult dup_handle(ErlNifEnv *env, fine::Term term) {
  Handle *handle = get_handle(env, term);
  if (handle == nullptr) {
    return fine::Error(atoms::not_local);
  }

  int fd;
  int error = 0;

  {
    std::lock_guard<std::mutex> lock(handle->mutex);

    if (!handle->open) {
      return fine::Error(atoms::closed);
    }

    fd = fcntl(handle->fd, F_DUPFD_CLOEXEC, 0);
    if (fd < 0) {
      error = errno;
    }
  }

  if (fd < 0) {
    return fine::Error(errno_atom(error));
  }

  ErlNifPid self;
  enif_self(env, &self);
  return make_handle(env, fd, self);
}

FINE_NIF(dup_handle, 0);

// Test hook: how many handles are allocated.
std::int64_t live_count(ErlNifEnv *) { return live_handles.load(); }

FINE_NIF(live_count, 0);

// Test hook: how many times close(2) ran for this handle.
std::variant<fine::Ok<std::uint64_t>, fine::Error<fine::Atom>>
close_count(ErlNifEnv *env, fine::Term term) {
  Handle *handle = get_handle(env, term);
  if (handle == nullptr) {
    return fine::Error(atoms::not_local);
  }

  return fine::Ok(handle->closes.load());
}

FINE_NIF(close_count, 0);

// Test hook: a pipe whose two ends are handles owned by the caller.
std::variant<fine::Ok<fine::Term, fine::Term>, fine::Error<fine::Atom>>
pipe_pair(ErlNifEnv *env) {
  int fds[2];

  if (::pipe(fds) != 0) {
    return fine::Error(errno_atom(errno));
  }

  fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  fcntl(fds[1], F_SETFD, FD_CLOEXEC);

  ErlNifPid self;
  enif_self(env, &self);

  auto read_end = make_handle(env, fds[0], self);
  auto write_end = make_handle(env, fds[1], self);

  // The caller is alive, so neither wrap can fail with :noproc.
  auto *read_ok = std::get_if<fine::Ok<fine::Term>>(&read_end);
  auto *write_ok = std::get_if<fine::Ok<fine::Term>>(&write_end);

  if (read_ok == nullptr || write_ok == nullptr) {
    return fine::Error(atoms::noproc);
  }

  return fine::Ok(std::get<0>(read_ok->items()),
                  std::get<0>(write_ok->items()));
}

FINE_NIF(pipe_pair, 0);

// Test hook: the device and inode behind a raw descriptor number, from
// fstat(2), so a test can tell which file a number refers to.
std::variant<fine::Ok<std::tuple<std::uint64_t, std::uint64_t>>,
             fine::Error<fine::Atom>>
fd_identity(ErlNifEnv *, std::int64_t fd) {
  if (fd < 0 || fd > INT_MAX) {
    return fine::Error(atoms::invalid_descriptor);
  }

  struct stat info;

  if (fstat(static_cast<int>(fd), &info) != 0) {
    return fine::Error(errno_atom(errno));
  }

  return fine::Ok(std::make_tuple(static_cast<std::uint64_t>(info.st_dev),
                                  static_cast<std::uint64_t>(info.st_ino)));
}

FINE_NIF(fd_identity, 0);

FINE_INIT("Elixir.Rebus.UnixFD.NIF");
