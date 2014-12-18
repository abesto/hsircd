#!/bin/bash
cabal build
./dist/build/hsircd/hsircd &
while ! nc -z 127.0.0.1 6697; do sleep 0.1; done
./dist/build/integration-test/integration-test
ret=$?
kill %
exit $ret
