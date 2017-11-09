#!/bin/bash
for elm in $@; do 
  stow -S $elm -t $HOME
done
