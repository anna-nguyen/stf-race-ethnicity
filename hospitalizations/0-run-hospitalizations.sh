#!/bin/bash

cd 1-analyze
bash 0-run-analysis.sh

cd ..
cd 2-figures
bash 0-run-figures.sh
