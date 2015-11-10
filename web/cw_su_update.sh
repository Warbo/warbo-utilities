#!/bin/sh
D=$(date +%s)
(cd rendered; ln -s /opt/git git)
mv /var/www/html "old/$D" && mv rendered /var/www/html
