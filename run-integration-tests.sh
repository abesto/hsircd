#!/bin/bash -ex
cabal build
./dist/build/hsircd/hsircd &
trap 'kill $(jobs -p)' SIGINT SIGTERM EXIT
while ! nc -z 127.0.0.1 6697; do sleep 0.1; done
./dist/build/integration-test/integration-test
ret=$?
exit $ret
