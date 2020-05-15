# shellcheck shell=bash

bazel_sanity_check() {
    local nixed_bazel_in_path
    local bazel_version_in_path
    local current_git_branch

    # Figuring out the Bazel version from the Nix store path doesn't require us
    # to actually run Bazel. However, if there are multiple Bazels in the $PATH
    # then we need to take the effective one, i.e. the one that would be picked
    # when the `bazel` command is invoked, hence the 'command -v' construct.  We
    # also need to make sure that user aliases aren't getting in our way, this
    # could have been achieved by using `which` instead of `command -v`, but
    # that's non-standard, and not available by default on MacOS.
    if ! nixed_bazel_in_path="$(unalias bazel &>/dev/null || true; command -v bazel)"; then
        fail "No 'bazel' command found in PATH. Try deleting the '.direnv/'" \
            "folder, followed by re-entering the repository, or calling 'direnv" \
            "allow'."
    fi

    if [[ ! "${nixed_bazel_in_path}" =~ ^/nix/store/.*-bazel-[.[:digit:]]+/bin/bazel$ ]]; then
        fail "The 'bazel' command doesn't come from Nix. If you have Bazel" \
            "installed on your (host) system, please, make sure that it" \
            "doesn't interfere with the monorepo environment."
    fi

    bazel_version_in_path="$(echo "${nixed_bazel_in_path%/*/*}" | cut -d- -f3)"

    current_git_branch="$(git rev-parse --abbrev-ref HEAD)"

    if [[ "${current_git_branch}" == 'master' ]] && [[ -d .direnv ]]; then
        echo "${bazel_version_in_path}" > .direnv/bazel-version-on-master
    elif [[ -f .direnv/bazel-version-on-master ]]; then
        local bazel_version_on_master
        bazel_version_on_master="$(< .direnv/bazel-version-on-master)"
        if [[ "${bazel_version_on_master}" != "${bazel_version_in_path}" ]]; then
            log_error "Version of 'bazel' on master" \
                "(${bazel_version_on_master}) is incompatible with this" \
                "branch's version (${bazel_version_in_path}).  Rebase before" \
                "continuing or things will likely break!"
        fi
    fi
}
