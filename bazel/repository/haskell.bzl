load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")

def setup_haskell():
    # Import external repositories that `rules_haskell` needs to work properly.
    rules_haskell_dependencies()

    nixpkgs_local_repository(
        name = "haskell_nixpkgs",
        nix_file = "//nix/haskell:default.nix",
    )

    nixpkgs_cc_configure(
        repository = "@haskell_nixpkgs",
    )

    haskell_register_ghc_nixpkgs(
        version = "8.6.5",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.ghc",
        compiler_flags = [
            "-Wall",
        ],
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_stack",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.stack",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_alex",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.alex",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_c2hs",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.c2hs",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_happy",
        repository = "@haskell_nixpkgs",
        attribute_path = "haskellPackages.happy",
    )

    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_zlib",
        repository = "@haskell_nixpkgs",
        attribute_paths = ["zlib_both.dev", "zlib_both.out"],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    stack_snapshot(
        name = "stackage",
        snapshot = "lts-14.4",
        stack = "@haskell_nixpkgs_stack//:bin/stack",
        packages = [
            # Core libraries
            "array",
            "base",
            "bytestring",
            "containers",
            "deepseq",
            "directory",
            "filepath",
            "ghc-heap",
            "mtl",
            "process",
            "text",
            "template-haskell",

            # Hackage dependencies
            "zlib",
        ],
        tools = [
            "@haskell_nixpkgs_alex//:bin/alex",
            "@haskell_nixpkgs_c2hs//:bin/c2hs",
            "@haskell_nixpkgs_happy//:bin/happy",
        ],
        extra_deps = {
            "zlib": [
                "@haskell_nixpkgs_zlib//:c_lib",
            ],
        },
    )

    # We shouldn't need this but otherwise using `bazel query` may fail.
    # See https://github.com/tweag/rules_haskell/issues/1078
    rules_haskell_worker_dependencies()

def nixpkgs_cc_library_package(
    name,
    repository,
    cc_library,
    attribute_paths = None,
    only_libs = None,
    **kwargs):

    if attribute_paths:
        nix_file_content = "\n".join([
            "with (import <nixpkgs> { config = {}; overlays = []; });",
            'buildEnv {name = "%s"; paths = [ %s ]; }' % (name, " ".join(attribute_paths)),
        ])
        kwargs = dict(kwargs, nix_file_content = nix_file_content)

    only_libs = only_libs or ["**/*"]
    libs = [paths.join("lib", lib) + ext for lib in only_libs for ext in [".so*", ".dylib", ".a"]]

    build_file_lines = (
        [
            'package(default_visibility = ["//visibility:public"])',
            "",
            "filegroup(",
            '    name = "bin",',
            '    srcs = glob(["bin/*"]),',
            ")",
            "",
            "filegroup(",
            '    name = "lib",',
            "    srcs = glob(%r)" % libs,
            ")",
            "",
            "filegroup(",
            '    name = "include",',
            '    srcs = glob(["include/**/*.h"]),',
            ")",
            "",
            "cc_library(",
        ] +
        ["    %s = %r," % (k, v) for k, v in cc_library.items()] +
        [")"]
    )

    nixpkgs_package(
        name = name,
        repository = repository,
        build_file_content = "\n".join(build_file_lines),
        **kwargs
    )
