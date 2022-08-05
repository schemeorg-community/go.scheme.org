#!/bin/sh
set -eu
cd "$(dirname "$0")"/..
set -x
mkdir -p nginx
mkdir -p www
gosh scripts/generate.scm
curl --location --fail --silent --show-error -o www/schemeorg.css \
    https://www.scheme.org/schemeorg.css
rsync -vcr --delete nginx/ alpha.servers.scheme.org:/production/go/nginx/
rsync -vcr --delete www/ alpha.servers.scheme.org:/production/go/www/
