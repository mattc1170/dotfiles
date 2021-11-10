#!/bin/sh

basedir=$PWD

echo "Linking git emacs dir to ~/.emacs.d"
if [ -h ~/.emacs.d ]; then
    echo "Removing existing ~/.emacs.d link"
    /bin/rm ~/.emacs.d
else
    if [ -d ~/.emacs.d ]; then
        echo "Moving existing ~/.emacs.d to .emacs.d.bak"
        mv ~/.emacs.d ~/.emacs.d.bak
    fi
fi
ln -s $PWD/emacs ~/.emacs.d

echo "Linking git .zshrc to ~"
if [ -h ~/.zshrc ]; then
    echo "Removing existing ~/.zshrc link"
    /bin/rm ~/.zshrc
else
    if [ -f ~/.zshrc ]; then
        echo "Moving existing ~/.zshrc to .zshrc.bak"
        mv ~/.zshrc ~/.zshrc.bak
    fi
fi
ln -s $PWD/zshrc ~/.zshrc

echo "Linking git .p10k.zsh to ~"
if [ -h ~/.p10k.zsh ]; then
    echo "Removing existing ~/.p10k.zsh link"
    /bin/rm ~/.p10k.zsh
else
    if [ -f ~/.p10k.zsh ]; then
        echo "Moving existing ~/.p10k.zsh to .p10k.zsh.bak"
        mv ~/.p10k.zsh ~/.p10k.zsh.bak
    fi
fi
ln -s $PWD/p10k.zsh ~/.p10k.zsh

echo "Linking git .screenrc to ~"
if [ -h ~/.screenrc ]; then
    echo "Removing existing ~/.screenrc link"
    /bin/rm ~/.screenrc
else
    if [ -f ~/.screenrc ]; then
        echo "Moving existing ~/.screenrc to .screenrc.bak"
        mv ~/.screenrc ~/.screenrc.bak
    fi
fi
ln -s $PWD/screenrc ~/.screenrc

echo "Installing Powerlevel10k zsh theme"
if [ -d ~/powerlevel10k ]; then
    echo "Moving existing ~/powerlevel10k to ~/powerlevel10k.bak"
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/powerlevel10k

cd ~/.emacs.d/lisp/
echo "Installing extra elisp files"

if [ ! -f dired-single.el ]; then
    echo "Fetching dired-single.el"
    wget http://www.emacswiki.org/cgi-bin/wiki/download/dired-single.el
else
    echo "File dired-single.el already installed"
fi

if [ ! -f guess-offset.el ]; then
    echo "Fetching guess-offset.el"
    wget http://www.emacswiki.org/emacs/download/guess-offset.el
else
    echo "File guess-offset.el already installed"
fi

mkdir -p ~/.emacs.d/themes
cd ~/.emacs.d/themes/
if [ ! -f zenburn-theme.el ]; then
    echo "Fetching zenburn-theme.el"
    wget https://github.com/bbatsov/zenburn-emacs/raw/master/zenburn-theme.el
else
    echo "File zenburn.el already installed"
fi
if [ ! -f atom-one-dark-theme.el ]; then
    echo "Fetching atom-one-dark-theme.el"
    wget https://raw.githubusercontent.com/jonathanchu/atom-one-dark-theme/master/atom-one-dark-theme.el
else
    echo "File atom-one-dark-theme.el already installed"
fi

# echo "Linking bin scripts to ~/bin"
# cd ${basedir}/bin
# if [ ! -d ~/bin ]; then
#     mkdir -p ~/bin
#     for file in *
#     do
# 	ln -s $PWD/${file} ~/bin/${file}
# 	echo "Linked ${file}"
#     done
# else
#     for file in *
#     do
# 	if [ -h ~/bin/${file} ]; then
# 	    echo "Removing existing ${file} link"
# 	    /bin/rm ~/bin/${file}
# 	else
# 	    if [ -f ~/bin/${file} ]; then
# 		echo "Moving existing ${file} to ${file}.bak"
# 		mv ~/bin/${file} ~/bin/${file}.bak
# 	    fi
# 	fi
# 	ln -s $PWD/${file} ~/bin/${file}
# 	echo "Linked ${file}"
#     done
# fi
# cd ..

echo "Installation complete"
