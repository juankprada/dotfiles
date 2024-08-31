#!/usr/bin/env bash


WORKING_DIR=${PWD}

echo ">>> Backing up existing files"
BASHRC_FILE=$HOME/.bashrc
if [ -f "$BASHRC_FILE" ]; then
    echo ">>> Backing up .bashrc file -> .bashrc_bk"
    mv $BASHRC_FILE ~/.bashrc_bk
fi


echo ""
echo ">>> Stow Linux exclusive HOME files"
cd $WORKING_DIR/shared
stow -R home/  --target=$HOME --dotfiles -v --ignore='.DS_Store'
cd ..

echo ""
echo ">>> stow .local/bin"
cd $WORKING_DIR/shared
stow -R dot-local/ --target=$HOME/.local --dotfiles --ignore='.DS_Store'
chmod +x ~/.local/bin/*
cd ..

echo ""
echo ">>> stow .config"
cd $WORKING_DIR/shared
stow -R dot-config --target=$HOME/.config  --dotfiles --ignore='.DS_Store'
cd ..

echo ""
echo ">>> stow Shared HOME specific files"
cd $WORKING_DIR/shared
stow -R home --target=$HOME --dotfiles -v --ignore='.DS_Store'
cd ..

SHARED_CONFIG_DIR="${WORKING_DIR}/shared/dot-config"
if [ -d "$SHARED_CONFIG_DIR" ]; then
    cd $WORKING_DIR/shared
    echo ""
    echo ">>> stow Shared ~/.config files "
    stow -D dot-config --target=$HOME/.config  --dotfiles --ignore='.DS_Store'
    stow dot-config --target=$HOME/.config  --dotfiles --ignore='.DS_Store'
    cd ..
    echo ""
fi
