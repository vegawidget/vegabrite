#!/usr/bin/env bash
HERE=$(dirname $0)
cd $HERE

npm install
browserify in.js > ajv.js
uglifyjs ajv.js > ajv.min.js

rm -r node_modules
rm ajv.js
mv ajv.min.js ../../inst/js/