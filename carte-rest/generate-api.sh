#!/bin/bash
DIRNAME=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
echo 'Generate REST-API lib ...'
docker run --rm -v "${DIRNAME}:/local" openapitools/openapi-generator-cli generate \
    -i /local/carte-spec.yaml \
    -g haskell \
    -o /local/open-api/haskell
echo 'REST-API lib generated.'
