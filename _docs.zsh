#!/usr/local/bin/zsh

for f in $(ls **/*.hs)
do
    mkdir -p docs/src/`dirname $f`
    HsColour -css -anchor $f > "docs/src/`echo $f | sed s/\\\//./ | sed s/\.hs/.html/`"
done

haddock --html --title="Implementations of various lambda calculus based systems." \
    --odir=docs \
    --source-module="src/%{MODULE}.html" \
    --source-entity="src/%{MODULE}.html#%{NAME}" \
    Main
