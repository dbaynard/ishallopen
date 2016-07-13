#!/usr/bin/env sh

stack build deploy
$(stack path --local-install-root)/bin/deploy-app "$@"
