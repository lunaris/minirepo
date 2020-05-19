load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

def setup_docker():
   container_deps()
