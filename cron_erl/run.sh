#! /bin/bash
erl -make
erl -noshell -run test -run init stop
