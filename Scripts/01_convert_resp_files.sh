#!/bin/bash

cd Raw_data/F3_resp_data/o2_data

for file in *.csv; do iconv -f UTF-16LE -t UTF-8 <"$file" >"$file".tmp && mv "$file".tmp ../o2_data_utf8/"$file"; done