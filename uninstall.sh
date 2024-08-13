#!/usr/bin/env bash

ZSHRC_FILE=$HOME/.zshrc_bk
if [ -f "$ZSHRC_FILE" ]; then
    echo ">>> Restoring backed .zshrc file"
    mv $ZSHRC_FILE ~/.zshrc
fi

echo ">>> stow HOME specific files"
stow -D home/dot-zshrc


stow -D home/  --target=$HOME --dotfiles -v


echo ">>> stow wallpapers"
stow -D Pictures --target=$HOME/Pictures
