#!/bin/bash
## Deploy

rsync -rvzhu --progress kamiak.wsu.edu:/data/cas/bocinsky/git/asian_niche/OUTPUT/ ~/git/asian_niche/OUTPUT/
rsync -rvzhu --progress ~/git/asian_niche/ ~/Dropbox/asian_niche/