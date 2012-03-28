#!/bin/bash
erl +K true +A 5 -pa ebin -pa deps/*/ebin -config hello_world2 -s hello_world -sname helloworld2@localhost