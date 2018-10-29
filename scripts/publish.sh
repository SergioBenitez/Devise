#! /usr/bin/env bash
set -e

#
# Publishes the current versions of core, codegen, and lib to crates.io.
#

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LIB_DIR="${SCRIPT_DIR}/../lib"
CODEGEN_DIR="${SCRIPT_DIR}/../codegen"
CORE_DIR="${SCRIPT_DIR}/../core"

# Publish all the things.
for dir in "${CORE_DIR}" "${CODEGEN_DIR}" "${LIB_DIR}"; do
  pushd ${dir}
  echo ":::: Publishing '${dir}'..."
  cargo publish
  popd
done
