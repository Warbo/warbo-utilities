#!/usr/bin/env bash

if [[ -d rendered ]]
then
    D=$(date +%s)
    (cd rendered && ln -s /opt/git git)
    mv /var/www/html "old/$D" && mv rendered /var/www/html
else
    echo "No 'rendered' directory" 1>&2
fi
