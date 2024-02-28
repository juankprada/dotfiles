#!/bin/zsh

WORKING_DIR=${PWD}

ZSHRC_FILE=$HOME/.zshrc
if [ -f "$ZSHRC_FILE" ]; then
    echo ""
    echo ">>> Backing up .zshrc file -> .zshrc_bk"
    mv $ZSHRC_FILE ~/.zshrc_bk
fi

echo ""
echo ">>> stow MacOS exclusive HOME files"
pushd -q macos
stow -R home  --target=$HOME --dotfiles -v  --ignore='.DS_Store'
popd -q

echo ""
echo ">>> stow Shared HOME specific files"
pushd -q shared
stow -R home --target=$HOME --dotfiles -v --ignore='.DS_Store'
popd -q

#echo ""
#echo ">>> stow Auto Start specific files"
#mkdir -p $HOME/Library/LaunchAgents
#pushd -q macos
#stow -R LaunchAgents --target=$HOME/Library/LaunchAgents --ignore='.DS_Store'
#popd -q


MACOS_CONFIG_DIR="${WORKING_DIR}/macos/dot-config"
if [ -d "$MACOS_CONFIG_DIR" ]; then
    pushd -q macos
    echo ""
    echo ">>> stow MacOS exclusive ~/.config files "
    stow -R dot-config --target=$HOME/.config  --dotfiles --ignore='.DS_Store'
    popd -q

fi

SHARED_CONFIG_DIR="${WORKING_DIR}/shared/dot-config"
if [ -d "$SHARED_CONFIG_DIR" ]; then
    pushd -q shared
    echo ""
    echo ">>> stow Shared ~/.config files "
    stow -D dot-config --target=$HOME/.config  --dotfiles --ignore='.DS_Store'
    stow dot-config --target=$HOME/.config  --dotfiles --ignore='.DS_Store'
    popd -q
    echo ""
fi

echo ""
echo ">>> stow Wallpapers"
pushd -q shared
stow -R Pictures --target=$HOME/Pictures --ignore='.DS_Store'
popd -q
