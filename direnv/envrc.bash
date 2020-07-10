#!/usr/bin/env bash

# This file is symlinked as `.envrc` in the repository root, which will trigger
# `direnv` to activate if installed and allowed. `direnv` is a tool for changing
# the shell environment when entering or leaving a directory and in this
# repository is used for implicitly entering a Nix shell, which should minimise
# the effort required to get up and running when developing in this repository
# (and equally help ensure that everyone working on a repository is using the
# same tools).

set -euo pipefail

# We can't just early-exit here as direnv wants to run some stuff after this
# script has run its course.
if [[ "${IN_NIX_SHELL:-false}" != "pure" ]]; then
  self=$(readlink .envrc)
  here=${self%/*}

  export EXTERNAL_PATH=${EXTERNAL_PATH:-$PATH}  # NB. don't override to ensure it's external

  fail() {
    log_error "${@}"
    exit 1
  }

  # shellcheck source=direnv/modules/version_checks.bash
  source "${here}/modules/version_checks.bash"

  # We want watch_file, which is only available after version 2.8, and dump
  # Bash, which is only available after 2.18.
  assert_version_at_least "direnv" "2.18" "$(${direnv:-false} version)"

  import() {
    local module="${here}/modules/$1.bash"
    # shellcheck disable=SC1090
    source "${module}"
    watch_file "${module}"
  }

  # Re-import the version check module so that we can also watch it for future
  # changes.
  import version_checks

  # Nix 2.0 leaks memory while downloading things, so we want at least 2.1.
  assert_version_at_least "nix" "2.1" "$(nix --version | cut -d' ' -f3)"

  # A host Nix command will be used for upgrading Nix if present.
  HOST_NIX=$(command -v nix)
  export HOST_NIX

  # Bring tools provided by shell.nix into scope.

  # shellcheck disable=SC2034
  # NIXOPTS used in use_nix
  declare -a NIXOPTS=( \
      '--option' 'substituters' \
          'https://cache.nixos.org https://static-haskell-nix.cachix.org' \
      '--option' 'trusted-public-keys' \
          'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU=' \
  )

  # shellcheck source=direnv/modules/use_nix.bash
  import use_nix
  use nix shell.nix nix/tooling/default.nix

  export MONOREPO="$PWD"

  IFS=: read -r -a NIX_PATH_PARTS <<<"${NIX_PATH:-}"
  NIX_PATH=$(printf "%s:" "nixpkgs=$MONOREPO/nix/tooling" "monorepo=$MONOREPO" "${NIX_PATH_PARTS[@]/#nixpkgs=/external-nixpkgs=}")
  NIX_PATH="${NIX_PATH%:}"
  export NIX_PATH

  # Custom additions the path whilst we are in the direnv.
  PATH="$MONOREPO/bin:$PATH"

  # An example of setting up custom AWS configuration.
  # export AWS_CONFIG_FILE="$MONOREPO/.aws/config"

  # An example of setting up custom Kubernetes configuration.
  # touch "$MONOREPO/k8s/kubectl/config"
  # KUBECONFIG="$MONOREPO/k8s/kubectl/config:$(find "$MONOREPO/k8s/kubectl" -name '*.yaml' | paste -sd:):${KUBECONFIG:-}"
  # export KUBECONFIG

  # shellcheck source=direnv/modules/bazel_sanity_check.bash
  import bazel_sanity_check
  bazel_sanity_check

  # This fixes the "manpath list too long" errors that can occur when running
  # 'man'.  If the MANPATH environment variable is set then 'man' will use it,
  # but otherwise it figures out the search path by looking at the PATH variable
  # and applying some heuristics. This is problematic because the large number
  # of Nix packages in our PATH variable cause 'man' to build a very large
  # search path which exceeds the limit (roughly 1850 characters).  The
  # following sets MANPATH so 'man' will not build its search path by looking at
  # PATH, and if necessary removes paths from the beginning to ensure the length
  # does not exceed 1800.
  MANPATH="$(manpath --quiet | grep -Po '((?<=:)|^).{1,1750}$')" || true
  export MANPATH
fi
