#!/bin/bash

cd vaxcov
bash 0-run-vaxcov.sh

cd ..
cd hospitalizations
bash 0-run-hospitalizations.sh
