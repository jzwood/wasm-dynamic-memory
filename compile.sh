#!/usr/bin/env bash

bash format &&
elm make src/Main.elm --output=elm.js
