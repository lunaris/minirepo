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

    # http_archive(
    #     name = "rules_haskell",
    #     sha256 = "f23deee19fabd8317cd66ac531169e2f332bb9420676de4d1429384ebfd5d74a",
    #     strip_prefix = "rules_haskell-a9930c7856d251718613cca7a75f83aa911bf725",
    #     urls = ["https://github.com/tweag/rules_haskell/archive/a9930c7856d251718613cca7a75f83aa911bf725.tar.gz"],
    # )

    http_archive(
        name = "rules_haskell",
        sha256 = "63d725d0f7e2c4a5c32f3f2d90cff55e740597b581752e83a404bc652db0103c",
        strip_prefix = "rules_haskell-f8660ecbf4c2208af8e3dbb24a8234121b83af3d",
        urls = ["https://github.com/tweag/rules_haskell/archive/f8660ecbf4c2208af8e3dbb24a8234121b83af3d.tar.gz"],
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

    http_archive(
        name = "zlib",
        build_file_content = """
cc_library(
    name = "zlib",
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    visibility = ["//visibility:public"],
)
cc_library(name = "z", srcs = glob(["*.c"]), hdrs = glob(["*.h"]))
        """,
        sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
        strip_prefix = "zlib-1.2.11",
        urls = ["http://zlib.net/zlib-1.2.11.tar.gz"],
    )
