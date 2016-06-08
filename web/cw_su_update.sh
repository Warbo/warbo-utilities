#!/usr/bin/env bash

if [[ -d rendered ]]
then
    D=$(date +%s)
    (cd rendered && ln -s /opt/git git)
    mv -v /var/www/html "old/$D" && mv -v rendered /var/www/html
else
    echo "No 'rendered' directory" 1>&2
fi
