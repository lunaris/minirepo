load("@io_bazel_rules_docker//repositories:deps.bzl", rules_docker_deps = "deps")

def setup_docker():
   rules_docker_deps()
