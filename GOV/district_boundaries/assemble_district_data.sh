#!/bin/bash

for zipfile in $(ls zipped_district_data/); do
    unzip $zipfile
done
