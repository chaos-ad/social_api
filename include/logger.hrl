-ifndef(_logger_included).
-define(_logger_included, yeah).

-define(LOG_TRACE(Pattern, Args), logger:format(?MODULE, trace, self(), Pattern, Args)).
-define(LOG_DEBUG(Pattern, Args), logger:format(?MODULE, debug, self(), Pattern, Args)).
-define(LOG_INFO (Pattern, Args), logger:format(?MODULE, info,  self(), Pattern, Args)).
-define(LOG_WARN (Pattern, Args), logger:format(?MODULE, warn,  self(), Pattern, Args)).
-define(LOG_ERROR(Pattern, Args), logger:format(?MODULE, error, self(), Pattern, Args)).
-define(LOG_FATAL(Pattern, Args), logger:format(?MODULE, fatal, self(), Pattern, Args)).

-endif.
