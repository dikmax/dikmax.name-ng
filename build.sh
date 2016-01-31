#!/usr/bin/env bash

node_modules/postcss-cli/bin/postcss -c postcss.json styles/styles.css > css/style.css
