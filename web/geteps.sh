#!/usr/bin/env bash
curl "$1" | xidel -s -e '//div[@class="el-item"]/a/@href' -
