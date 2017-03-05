#!/bin/bash

while read cngrs; do
    wget "http://cdmaps.polisci.ucla.edu/shp/districts$cngrs.zip" 
done < congresses.txt
