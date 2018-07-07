#!/bin/sh

basedir=$PWD

echo "Creating ~/.emacs.d/lisp directory"
mkdir -p ~/.emacs.d/lisp

echo "Installing custom.el"
if [ -f ~/.emacs.d/lisp/custom.el -a ! -h ~/.emacs.d/lisp/custom.el ]; then
    echo "Moving existing ~/.emacs.d/lisp/custom.el to custom.el.bak"
    mv ~/.emacs.d/lisp/custom.el ~/.emacs.d/lisp/custom.el.bak
fi
cp $PWD/custom.el ~/.emacs.d/lisp

if [ ! -f ~/.emacs.d/lisp/local.el ]; then
    echo "Creating empty local.el"
    touch ~/.emacs.d/lisp/local.el
fi

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

echo "Linking  .emacs to ~"
if [ -h ~/.emacs ]; then
    echo "Removing existing ~/.emacs link"
    /bin/rm ~/.emacs
else
    if [ -f ~/.emacs ]; then
        echo "Moving existing ~/.emacs to emacs.bak"
        mv ~/.emacs ~/.emacs.bak
    fi
fi
ln -s $PWD/emacs ~/.emacs

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

