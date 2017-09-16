;; start server process
(require 'server)
(unless (server-running-p)
  (server-start))
