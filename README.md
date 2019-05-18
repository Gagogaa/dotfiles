# Overview
These are my dotfiles.
Configurations for the various applications that I use on my systems.
Each directory contains configuration files for a specific application.
To install my dotfiles first install GNU stow.
It will look something like this:

```sh
sudo apt install stow -y
```
To make things easy I've included an `install.sh` script to install multiple configurations at once.
It can be used like so:

```sh
./install.sh bash bin emacs nvim
```

Now only a few of these directories are meant to be installed using stow, namely the `bash`, `emacs`, `bin`, and `nvim` directories.
The `extra` directory contains other configuration files where I describe its purpose and installation process at the top of the file.
The `windows` directory contains configuration files that are used by Windows applications, for the seldom times I have to use Windows.

## nvim
My neovim configuration needs a few more steps to configure it properly.
We need to install `Vundle` so we can use it to manage all the other packages.
Vundle's installation instructions can be found at its [github page]( https://github.com/VundleVim/Vundle.vim ).

Or just run this:
```sh
git clone https://github.com/VundleVim/Vundle.vim.git ~/.config/nvim/bundle/Vundle.vim
```

After installing Vundle starting vim will still throw errors.
Ignore them and run `:VundleInstall` to install the managed packages.

## bin
The `bin` directory contains scripts that are held together by pretzel sticks and marshmallows.
If they do not work consult the offending script for documentation near the top of the file.
