#!/bin/bash
cd "$(dirname "$0")"
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
