#!/bin/bash

for zipfile in $(ls | grep .zip); do
    unzip $zipfile
    
done
