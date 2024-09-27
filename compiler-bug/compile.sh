#!/usr/bin/env bash

elm-format src/ --yes &&
elm make src/Main.elm --output=elm.js
