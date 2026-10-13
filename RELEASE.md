### Changed

- Connections that negotiate Unix file descriptor passing now read inbound
  messages in bulk, as other connections do, rather than with at least two
  reads per message. Received descriptors go to the messages that begin in the
  same read, in order, according to each message's declared descriptor count.
  Descriptors that no such message claims are closed and logged.
