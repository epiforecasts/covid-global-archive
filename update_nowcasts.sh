#!/bin/bash

## Make sure everything is up to date
git pull

## Run the world
Rscript update_nowcasts.R

## Update master with then new results
git add --all
git commit -m "Updated nowcasts"
git push
