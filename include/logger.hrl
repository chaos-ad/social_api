-ifndef(_logger_included).
-define(_logger_included, yeah).

-define(LOG_INFO (Pattern, Args), error_logger:info_msg(Pattern ++ "~n", Args)).
-define(LOG_WARN (Pattern, Args), error_logger:warning_msg(Pattern ++ "~n", Args)).
-define(LOG_ERROR(Pattern, Args), error_logger:error_msg(Pattern ++ "~n", Args)).
-define(LOG_FATAL(Pattern, Args), error_logger:error_msg(Pattern ++ "~n", Args)).

-endif.
