#!/bin/bash
cd `dirname $0`
exec erl -pa ebin deps/*/ebin -boot start_sasl -config priv/app.config -sname social_apisrv@`hostname` -cookie social_api_cookie -s reloader -s social_api
