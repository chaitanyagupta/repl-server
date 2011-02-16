(ql:quickload :repl-server)

(repl-server:start-server)

(repl-server:start-repl :color-output t :exit-on-finish t)
