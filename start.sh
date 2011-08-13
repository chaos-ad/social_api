#!/bin/bash
export ERL_LIBS="./deps/:../:${ERL_LIBS}"
exec erl -pa ebin -boot start_sasl -s reloader -s social_api -sname social_api@`hostname` -config priv/example

