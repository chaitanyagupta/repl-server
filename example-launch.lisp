(ql:quickload :repl-server)

(repl-server:defcolor :repl :style :bright)
(repl-server:defcolor :result :fg :white)
(repl-server:defcolor :info :style :dim :fg :reset)
(repl-server:defcolor :warn :fg :yellow)
(repl-server:defcolor :error :fg :red)

(repl-server:start-server)

(repl-server:start-repl :color-output t :exit-on-finish t)
