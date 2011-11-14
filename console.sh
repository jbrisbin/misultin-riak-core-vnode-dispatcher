#!/bin/bash
erl -pa ebin -pa deps/*/ebin -s hello_world -name helloworld@localhost