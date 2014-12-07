# Overview

At a [Clojadelphia](http://www.meetup.com/Clojadelphia/) meeting, a
few people asked me to give a lightning talk on how I use Emacs.

What I want to get across in this talk is that Emacs is extensible,
you can extend it to improve your productivity and should.

Emacs is designed very much in the lisp spirit of "build the language
up toward your program".  With Emacs, you can "build the IDE up toward
your development process".


# Running the code

Download [bake](https://github.com/kyleburton/bake/)


    $ test -d $HOME/bin || mkdir $HOME/bin
    $ curl https://raw.githubusercontent.com/kyleburton/bake/master/bake > $HOME/bin/bake
    $ chmod 755 $HOME/bin/bake

Run

    bake install

This will install the following packages into ./software

* CIDER
* AutoComplete Cider
* leiningen into $HOME/bin/


# Slides

## <quote>If you're _using_ Emacs, you're doing it wrong</quote>

or

## <quote>You shouldn't be _using_ Emacs</quote>


