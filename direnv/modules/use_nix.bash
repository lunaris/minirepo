# This is based on : https://github.com/direnv/direnv/wiki/Nix#persistent-cached-shell

# Usage: use_nix [<shell.nix> [dep1 [...[dep n]]] [-- [...]]
#
# Load environment variables from `nix-shell`. The mandatory first argument will
# be used to the derive an environment, which will be stored at
#  ./.direnv/env-<hash>  and symlink to it will be created at ./.direnv/default.
# Dependencies are added to the GC roots, such that the environment remains persistent.
# Parameters after `--` will be passed to `nix-shell`.
# The resulting environment is cached for better performance.
#
# Above, the hash is computed from the provided <shell.nix> file and all optional
# `depi` dependencies (typically `nixpkgs.json` or any other nix files on which
# `shell.nix` depends and whose modification should trigger a new environment
#  to be derived.

# NB. May be set from the outside to request a pure env. In any case, we export
# it so we can know whether the environment is supposed to be pure or not.
export NIXENV_PURE
NIXENV_PURE=${NIXENV_PURE:-false}
[[ "${NIXENV_PURE}" == true ]] || NIXENV_PURE=false

use_nix() {
    set -e

    local -a shell_inputs

    while [[ $# -gt 0 ]]; do
      case "$1" in
        "--")
          shift
          break
          ;;
        *)
          [[ -f $1 ]] || fail "use nix: file not found '$1'"
          shell_inputs+=("$1")
          shift
          ;;
      esac
    done

    if  [[ ${#shell_inputs[*]} -eq 0 ]]; then
      if [[ -f "shell.nix" ]]; then
        shell_inputs+=("shell.nix")
      elif [[ -f "default.nix" ]]; then
        shell_inputs+=("default.nix")
      else
        fail "use nix: shell.nix or default.nix not found in the folder"
      fi
    fi

    local shell="${shell_inputs[0]}"

    local dir="${NIXENV_CACHE:-${PWD}/.direnv}"
    local default="${dir}/default"

    local wd
    wd="${dir}/env-$(hash_files "${shell_inputs[@]}")"

    if [[ ! -L "${default}" || "${dir}/$(readlink "${default}")" != "${wd}" || ! -d "${wd}" ]]; then
        mkdir -p "${wd}"

        local drv="${wd}/env.drv"
        if [[ ! -f "${drv}" ]]; then
            log_status "use nix: deriving new environment"

            # Normally we could use [[ -v NIXOPTS ]], but this doesn't work on
            # the prehistoric version of Bash shipped with OSX....
            [[ -n "${NIXOPTS+1}" ]] || local -a NIXOPTS=( '' )

            IN_NIX_SHELL=1 nix-instantiate "${NIXOPTS[@]}" --add-root "${drv}" --indirect "${shell}" > /dev/null

            local -a drvs=()
            while IFS='' read -r line; do drvs+=("$line"); done < <(nix-store --query --references "${drv}")

            nix-store "${NIXOPTS[@]}" --realise "${drvs[@]}" --add-root "${wd}/dep" --indirect > /dev/null
        fi


        # If we Ctrl-C during the `nix-store --realise`, we get an exit status
        # of 0 (*sigh*), so we try to guess that the realisation was successful
        # by checking that the outcome looks sensible
        if [[ -f "${wd}/env.drv" && -f "${wd}/dep" ]]; then
          rm -f "${default}"
          ln -s "$(basename "${wd}")" "${default}"
        else
          # Clean up on error, so that we can retry.
          rm -Rf "${wd}"
        fi
    fi

    local env_drv
    env_drv=$(readlink "${default}/env.drv")

    local dump
    dump="${dir}/dump-$(hash_files ".envrc" "${env_drv}")"

    if [[ -e "${env_drv}" && ! -f "${dump}" ]]; then
        log_status "use nix: updating cache"

        local all_exported
        all_exported="declare -x | grep  -E '^(declare -x )\w+=' | cut -d' ' -f3 | cut -d= -f1"

        local whitelisted_vars='grep -Ev "^(_|XDG|TE?MP)" | grep -E "PATH|DIR|LOCALE_ARCHIVE|GIT_SSL_CA|HABIT_|COMMAND_WRAPPER_"'

        nix-shell "${env_drv}" --show-trace --pure "$@" > "${dump}.tmp" \
          --run "to_keep=\$(${all_exported} | ${whitelisted_vars}); export -n \$(${all_exported}); export \$to_keep; \"${direnv:-false}\" dump bash"

        # Prevent corrupted dump files when nix-shell fails.
        mv "${dump}.tmp" "${dump}"

        find "${dir}" -name 'dump-*' ! -samefile "${dump}" -ctime 15 -delete
    fi

    if [[ -f "${dump}" ]]; then
      local path_backup=$PATH
      eval "$(< "${dump}")"
      if ! "${NIXENV_PURE}"; then
        PATH=$PATH:$path_backup
      fi
    fi

    for f in "${shell_inputs[@]}"; do
      watch_file "$f"
    done
}

hash_files() {
  if has md5sum; then
    md5sum "${@}" | md5sum | cut -c -32
  elif has md5; then
    md5 "${@}" | md5 -q
  fi
}
