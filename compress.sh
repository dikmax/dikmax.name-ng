#!/bin/sh

ls *.jpg | parallel -j-1 guetzli --quality 85 --verbose {} {}
