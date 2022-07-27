#!/bin/bash

echo 'Generate REST-API lib ...'
docker run --rm -v "${PWD}:/local" openapitools/openapi-generator-cli generate \
    -i /local/carte-spec.yaml \
    -g haskell \
    -o /local/open-api/haskell
echo 'REST-API lib generated.'
