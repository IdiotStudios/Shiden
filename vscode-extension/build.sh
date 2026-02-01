#!/usr/bin/env bash

# Visual Studio Code
npx vsce package


cd vscode-extension
npx vsce package

exit