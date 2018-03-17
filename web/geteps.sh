#!/usr/bin/env bash
curl "$1" | xidel -q -e '//div[@class="el-item"]/a/@href' -
