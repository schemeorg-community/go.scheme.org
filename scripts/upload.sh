#!/bin/sh
set -eu
cd "$(dirname "$0")"/..
set -x
mkdir -p nginx
mkdir -p www
gosh scripts/generate.scm
curl --location --fail --silent --show-error -o www/schemeorg.css \
    https://www.scheme.org/schemeorg.css
rsync -vcr --delete nginx/ tuonela.scheme.org:/production/go/nginx/
rsync -vcr --delete www/ tuonela.scheme.org:/production/go/www/
ssh tuonela.scheme.org sudo service nginx restart
