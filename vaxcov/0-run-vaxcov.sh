#!/bin/bash
cd 1-dm
bash 0-run-cleaning.sh

cd..
cd 2-analysis
bash 0-run-analysis.sh

cd ..
cd 3-tab-fig
bash 0-run-tab-fig.sh
