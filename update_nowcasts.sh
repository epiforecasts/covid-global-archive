#!/bin/bash

## Run the world
Rscript update_nowcasts.R

## Run death Rts for the world
Rscript update_deaths_nowcasts.R