#!/bin/bash

SOURCE_DIR="$( cd "$( dirname "$( realpath . )" )" && pwd )"
VENV_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/nvim/python3"

echo "Using python3 from: $( which python3 )"

if [[ -f $VENV_DIR/bin/activate ]]; then
    echo "Found venv dir at $VENV_DIR"
else
    python3 -m venv $VENV_DIR
fi

source $VENV_DIR/bin/activate
echo "pip is now at $( which pip )"

pip install --upgrade pip
pip install --upgrade pynvim
pip install --upgrade git+https://github.com/psf/black.git
pip install --upgrade jedi-language-server

NVIM_VERSION="$( nvim --version | awk '/NVIM v/ { print $2 }' | tr -d "v" )"

if [[ $NVIM_VERSION < 0.5.0 ]]; then
    echo "Warning: neovim version seems to be < 0.5.0" >&2
fi

deactivate
