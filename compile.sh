#!/usr/bin/env bash

elm-format App.elm --yes
elm make App.elm --output=elm.js
