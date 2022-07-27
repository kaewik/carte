#!/bin/bash -e
DIRNAME=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")

echo 'Validate REST-API specification ...'
docker run --rm -v "${DIRNAME}:/local" openapitools/openapi-generator-cli validate \
    -i /local/carte-spec.yaml
echo 'REST-API specification validated.'

echo 'Generate REST-API lib ...'
docker run --rm -v "${DIRNAME}:/local" openapitools/openapi-generator-cli generate \
    -i /local/carte-spec.yaml \
    -g haskell \
    -o /local/open-api
echo 'REST-API lib generated.'
