#!/bin/bash
function linkString {
  ls $1 | grep ".asd$" | cat | while read line; do echo ' ln -s ' $PWD/$1/$line 'systems/burning-'$line; done 
}

ls src/ | cat | while read line; do linkString src/$line; done | sh
