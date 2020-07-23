load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def workspace_dependencies():
    http_archive(
        name = "bazel_skylib",
        sha256 = "1dde365491125a3db70731e25658dfdd3bc5dbdfd11b840b3e987ecf043c7ca0",
        url = "https://github.com/bazelbuild/bazel-skylib/releases/download/0.9.0/bazel_skylib-0.9.0.tar.gz",
    )

    http_archive(
        name = "io_tweag_rules_nixpkgs",
        sha256 = "5c80f5ed7b399a857dd04aa81e66efcb012906b268ce607aaf491d8d71f456c8",
        strip_prefix = "rules_nixpkgs-0.7.0",
        urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.7.0.tar.gz"],
    )

    http_archive(
        name = "rules_sh",
        sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
        strip_prefix = "rules_sh-0.2.0",
        urls = ["https://github.com/tweag/rules_sh/archive/v0.2.0.tar.gz"],
    )

    git_repository(
        name = "rules_python",
        commit = "4b84ad270387a7c439ebdccfd530e2339601ef27",
        remote = "https://github.com/bazelbuild/rules_python.git",
        shallow_since = "1564776078 -0400",
    )

    http_archive(
        name = "rules_haskell",
        sha256 = "dc21a9b30734ab0fce58c2107656aac004a92c3caef913e54082335abc7cf468",
        strip_prefix = "rules_haskell-a5a550874d5be8852755bd9bfe3f6d9a8d3ba793",
        urls = ["https://github.com/tweag/rules_haskell/archive/a5a550874d5be8852755bd9bfe3f6d9a8d3ba793.tar.gz"],
    )

    http_archive(
        name = "io_bazel_rules_go",
        urls = [
            "https://storage.googleapis.com/bazel-mirror/github.com/bazelbuild/rules_go/releases/download/v0.20.2/rules_go-v0.20.2.tar.gz",
            "https://github.com/bazelbuild/rules_go/releases/download/v0.20.2/rules_go-v0.20.2.tar.gz",
        ],
        sha256 = "b9aa86ec08a292b97ec4591cf578e020b35f98e12173bbd4a921f84f583aebd9",
    )

    http_archive(
        name = "io_bazel_rules_docker",
        sha256 = "dc97fccceacd4c6be14e800b2a00693d5e8d07f69ee187babfd04a80a9f8e250",
        strip_prefix = "rules_docker-0.14.1",
        urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.14.1/rules_docker-v0.14.1.tar.gz"],
    )
