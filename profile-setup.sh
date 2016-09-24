#!/bin/bash

cp .vimrc ~

# Install Pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

cd=${PWD}
cd ~/.vim/bundle

# Vim plugins
git clone https://github.com/epeli/slimux.git
cd $cwd
