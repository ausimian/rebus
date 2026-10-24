# Builds the Rebus.UnixFD NIF. `mix compile` runs this through elixir_make,
# which sets MIX_APP_PATH, ERTS_INCLUDE_DIR and FINE_INCLUDE_DIR (see
# make_env/0 in mix.exs).
#
# `mix elixir_make.precompile` also sets CC_PRECOMPILER_CURRENT_TARGET, and
# CC and CXX for that target, to build the libraries CI publishes. Each target
# gets its own object directory, so neither a source build nor the other
# macOS target leaves objects that a precompiled build would reuse.

PRECOMPILE_TARGET := $(CC_PRECOMPILER_CURRENT_TARGET)

PRIV_DIR  := $(MIX_APP_PATH)/priv
BUILD_DIR := $(MIX_APP_PATH)/obj$(if $(PRECOMPILE_TARGET),/$(PRECOMPILE_TARGET))
NIF_SO    := $(PRIV_DIR)/rebus_unix_fd.so

SOURCES := $(wildcard c_src/*.cpp)
OBJECTS := $(patsubst c_src/%.cpp,$(BUILD_DIR)/%.o,$(SOURCES))

CXXFLAGS := -std=c++17 -O2 -fPIC -fvisibility=hidden -Wall -Wextra
CXXFLAGS += -I$(ERTS_INCLUDE_DIR)
# Third-party headers: -isystem keeps their warnings out of our builds.
CXXFLAGS += -isystem $(FINE_INCLUDE_DIR)

# CI sets REBUS_NIF_WERROR=1. Installs do not, so a newer compiler's new
# warning cannot break a dependency build.
ifeq ($(REBUS_NIF_WERROR),1)
    CXXFLAGS += -Werror
endif

LDFLAGS := -shared

ifeq ($(shell uname -s),Darwin)
    LDFLAGS += -undefined dynamic_lookup
endif

# A precompiled library must load on systems older than the one that built
# it. On Linux it carries its own C++ runtime, so it needs nothing newer than
# the C library of the image that built it, and it keeps that runtime's
# symbols to itself; unused sections and the symbol table go, which keeps it
# small. On macOS it targets macOS 11, the first release for Apple silicon.
ifneq ($(findstring linux,$(PRECOMPILE_TARGET)),)
    CXXFLAGS += -ffunction-sections -fdata-sections
    LDFLAGS  += -static-libstdc++ -static-libgcc -Wl,--gc-sections -Wl,--exclude-libs,ALL -s
endif
ifneq ($(findstring darwin,$(PRECOMPILE_TARGET)),)
    CXXFLAGS += -mmacosx-version-min=11.0
    LDFLAGS  += -mmacosx-version-min=11.0 -Wl,-dead_strip -Wl,-x
endif

.PHONY: all clean cppcheck

all: $(NIF_SO)

$(BUILD_DIR)/%.o: c_src/%.cpp Makefile
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(NIF_SO): $(OBJECTS) Makefile
	@mkdir -p $(PRIV_DIR)
	$(CXX) $(OBJECTS) -o $@ $(LDFLAGS)

clean:
	rm -rf $(BUILD_DIR) $(NIF_SO)

# Static analysis of the NIF source. Self-contained: it needs neither the
# ERTS nor the Fine headers, so `make cppcheck` runs on a bare checkout.
# The missing-include notices for <erl_nif.h> and <fine.hpp> are suppressed,
# which scopes findings to code in c_src/.
#
# `passedByValue` is suppressed because each NIF takes its arguments by
# value: FINE_NIF decodes every term into a value and passes it in.
CPPCHECK      ?= cppcheck
CPPCHECK_JOBS ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)

cppcheck:
	$(CPPCHECK) --enable=warning,performance,portability \
	  --std=c++17 --language=c++ \
	  --inline-suppr --error-exitcode=1 --quiet -j $(CPPCHECK_JOBS) \
	  --suppress=missingInclude --suppress=missingIncludeSystem \
	  --suppress=unmatchedSuppression --suppress=passedByValue \
	  $(SOURCES)
