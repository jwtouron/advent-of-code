#!/usr/bin/env bash

WINDOWS_EXE=dist-newstyle/build/x86_64-windows/ghc-8.10.2/haskell-0.1.0.0/x/advent-of-code-y2020/build/advent-of-code-y2020/advent-of-code-y2020.exe

if [[ -f $WINDOWS_EXE ]]; then
    $WINDOWS_EXE $*
fi

LINUX_EXE=dist-newstyle/build/x86_64-linux/ghc-8.10.2/advent-of-code-y2020-0.1.0.0/x/advent-of-code-y2020/build/advent-of-code-y2020/advent-of-code-y2020

if [[ -f $LINUX_EXE ]]; then
    $LINUX_EXE $*
fi
