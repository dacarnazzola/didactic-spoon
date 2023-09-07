#!/bin/bash

clear

rm -rf bin/ build/

cmake --preset=debug
cmake --build build

./bin/test_atmosphere

./bin/test_random
