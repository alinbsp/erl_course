#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname ebank_dev \
    -s ebank \
    -s reloader
