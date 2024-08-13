#!/usr/bin/env bash

# if [[ -z $STOW_FOLDERS ]]; then
#     STOW_FOLDERS="bashrc,Xresources,emacs,tmux,xsession,gtk-2.0"
# fi

# if [[ -z $DOTFILES ]]; then
#     DOTFILES=$(pwd)
# fi

# STOW_FOLDERS=$STOW_FOLDERS

# pushd $DOTFILES
# for folder in $(echo $STOW_FOLDERS | sed "s/,/ /g")
# do
#     echo ">>> stow $folder"
#     stow -D $folder --target ~/ --dotfiles
#     stow $folder --target ~/ --dotfiles
# done
# popd
WORKING_DIR=${PWD}

echo ">>> Backing up existing files"
BASHRC_FILE=$HOME/.bashrc
if [ -f "$BASHRC_FILE" ]; then
    echo ">>> Backing up .bashrc file -> .bashrc_bk"
    mv $BASHRC_FILE ~/.bashrc_bk
fi

GTK_FILE=$HOME/.gtkrc-2.0
if [ -f "$GTK_FILE" ]; then
    echo ">>> Backing up .gtkrc-2.0 file -> .gktrc-2.0_bk"
    mv $GTK_FILE ~/.gtkrc-2.0_bk
fi



echo ""
echo ">>> Stow Linux exclusive HOME files"
cd $WORKING_DIR/linux
stow -R home/  --target=$HOME --dotfiles -v --ignore='.DS_Store'
cd ..

echo ""
echo ">>> stow .local/bin"
cd $WORKING_DIR/linux
stow -R dot-local/ --target=$HOME/.local --dotfiles
chmod +x ~/.local/bin/*
cd ..

echo ""
echo ">>> stow .config"
cd $WORKING_DIR/linux
stow -R dot-config --target=$HOME/.config  --dotfiles
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

echo ""
echo ">>> stow wallpapers"
cd $WORKING_DIR/shared
stow -R Pictures --target=$HOME/Pictures
cd ..

echo ""
echo ">>> stow Fonts"
cd $WORKING_DIR/shared
stow -R fonts --target=$HOME/.local/share/fonts
cd ..
echo ""
