-module(test_log).
-export([init/0,log/0]).
init()->
    init_log4erl().

log()->
    log4erl:warn("warn3 msg").

init_log4erl() ->
    application:start(log4erl),
    log4erl:add_file_appender(app,{"../logs", "app", {size, 100000}, 10, log, info}),
    log4erl:change_format(app, "%j %T [%L] %l%n"),
    log4erl:add_console_appender(cmd_logs, {info, "%j %T [%L] %l%n"}).
