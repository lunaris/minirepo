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
        nix_file_content = """
          with import <nixpkgs> { config = {}; overlays = []; }; buildEnv {
            name = "bazel-cc-toolchain";
            paths = [ staticHaskell.stdenv.cc staticHaskell.binutils ];
          }
        """,
    )

    haskell_register_ghc_nixpkgs(
        version = "8.6.5",
        repository = "@haskell_nixpkgs",
        attribute_path = "staticHaskell.haskell.compiler.ghc865",
        compiler_flags = [
            "-Wall",
        ],
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_stack",
        repository = "@haskell_nixpkgs",

        # We don't need `stack` to be built statically (it's a build-time
        # dependency) so we take it from the default `haskellPackages`, which is
        # more likely to a. work and b. be cached.
        attribute_path = "haskellPackages.stack",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_alex",
        repository = "@haskell_nixpkgs",

        # We don't need `alex` to be built statically (it's a build-time
        # dependency) so we take it from the default `haskellPackages`, which is
        # more likely to a. work and b. be cached.
        attribute_path = "haskellPackages.alex",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_c2hs",
        repository = "@haskell_nixpkgs",

        # We don't need `c2hs` to be built statically (it's a build-time
        # dependency) so we take it from the default `haskellPackages`, which is
        # more likely to a. work and b. be cached.
        attribute_path = "haskellPackages.c2hs",
    )

    nixpkgs_package(
        name = "haskell_nixpkgs_happy",
        repository = "@haskell_nixpkgs",

        # We don't need `happy` to be built statically (it's a build-time
        # dependency) so we take it from the default `haskellPackages`, which is
        # more likely to a. work and b. be cached.
        attribute_path = "haskellPackages.happy",
    )

    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_postgresql",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.postgresql",
            "staticHaskell.postgresql.lib",
        ],
				libs = [
					"lib/libpq.so*",
					"lib/libpq.dylib",
					"lib/libpq.a",
				],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    # Pulling in OpenSSL from Nixpkgs
    #
    # There are a couple of considerations when pulling in OpenSSL from Nixpkgs
    # to make things work:
    #
    # * While both `libcrypto` and `libssl` come from `openssl`, we bring them in
    #   separately. This is because they must be linked in a particular order
    #   (`libssl` must be linked _before_ `libcrypto`) when static linking on `.a`
    #   files is being performed (see e.g.
    #   https://github.com/nutechsoftware/ser2sock/pull/13/files), and having two
    #   separate Bazel targets allows us to achieve this without excessive hacking
    #   (see the order these targets are passed as `extra_deps` to libraries that
    #   need them).
    #
    # * We pull in a custom derivation, `openssl_both`, that includes both
    #   dynamic (`*.so`, etc.) and static (`*.a`) libraries. This is so that
    #   both building (GHC, static dependencies) and REPLs (GHCi, dynamic
    #   dependencies without hacking GHCi) work. This is in the vein of the
    #   `zlib_both` package, which is already provided by `static-haskell-nix`.
    #
    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_crypto",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.openssl_both.dev",
            "staticHaskell.openssl_both.out",
        ],
        libs = [
          "lib/libcrypto.*",
        ],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_openssl",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.openssl_both.dev",
            "staticHaskell.openssl_both.out",
        ],
        libs = [
          "lib/libssl.*",
        ],
        cc_library = dict(
            name = "c_lib",
            srcs = [":lib"],
            hdrs = [":include"],
            strip_include_prefix = "include",
            visibility = ["//visibility:public"],
            linkstatic = True,
        ),
    )

    nixpkgs_cc_library_package(
        name = "haskell_nixpkgs_zlib",
        repository = "@haskell_nixpkgs",
        attribute_paths = [
            "staticHaskell.zlib_both.dev",
            "staticHaskell.zlib_both.out",
        ],
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
        snapshot = "lts-14.27",
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
            "postgresql-simple",
            "zlib",
        ],
        tools = [
            "@haskell_nixpkgs_alex//:bin/alex",
            "@haskell_nixpkgs_c2hs//:bin/c2hs",
            "@haskell_nixpkgs_postgresql//:bin/pg_config",
            "@haskell_nixpkgs_happy//:bin/happy",
        ],
        extra_deps = {
            "postgresql-libpq": [
                # Note that as per
                # https://github.com/nh2/static-haskell-nix/issues/57, we need
                # to link against `openssl` whenever we link against `libpq`.
                # Moreover, note the order in which we link `libcrypto` and
                # `libssl` is important -- see "Pulling in OpenSSL from Nixpkgs"
                # for more information.
                #
                # DO NOT REORDER THESE TWO DEPENDENCIES.
                #
                "@haskell_nixpkgs_openssl//:c_lib",
                "@haskell_nixpkgs_crypto//:c_lib",

                "@haskell_nixpkgs_postgresql//:c_lib",
            ],
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
    libs = None,
    **kwargs):

    if attribute_paths:
        nix_file_content = "\n".join([
            "with (import <nixpkgs> { config = {}; overlays = []; });",
            'buildEnv {name = "%s"; paths = [ %s ]; }' % (name, " ".join(attribute_paths)),
        ])
        kwargs = dict(kwargs, nix_file_content = nix_file_content)

    # If an explicit set of `libs` is given, use those. Otherwise, glob all
    # dynamic and static libraries (including those we might find on
    # Darwin/macOS).
    libs = libs or [paths.join("lib", "**/*") + ext for ext in [".so*", ".dylib", ".a"]]

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
