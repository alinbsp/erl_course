#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname /home/visan/erl_course/ebank_dev \
    -s /home/visan/erl_course/ebank \
    -s reloader
