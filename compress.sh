#!/bin/sh

ls *.png | parallel -j4 guetzli --quality 85 --verbose {} {}
