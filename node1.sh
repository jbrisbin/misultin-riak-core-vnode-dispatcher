#!/bin/bash
erl +K true +A 5 -pa ebin -pa deps/*/ebin -config hello_world1 -s hello_world -sname helloworld1@localhost