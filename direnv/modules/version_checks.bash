assert_version_at_least() {
  local command="$1"
  local min_version="$2"
  local command_version="$3"

  [[ -n "${min_version}" && -n "${command_version}" ]] || \
    fail "Bad check: assert_version_at_least '${command}' '${min_version}' '${command_version}'"

  local higher_version
  higher_version="$(printf "%s\n%s\n" "${min_version}" "${command_version}" | sort -V -r | head -n1)"

  if [[ "${higher_version}" != "${command_version}" ]]; then
    fail "Found ${command##*/}-${command_version} which is too old -- version ${min_version} or later required!"
  fi
}
