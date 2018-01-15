# ML4PG usage instructions #

ML4PG performs machine learning on Coq proof scripts, finding similarities in
theorems/lemmas/etc. and clustering them. For more information, see
ml4pg_manual.pdf

## Quick Start ##

ML4PG is maintained at http://chriswarbo.net/essays/repos/ml4pg.html where you
can check out the latest version using git.

Once you've downloaded ML4PG, the easiest way to use it is via the Nix package
manager. If your operating system doesn't provide Nix, you can get it manually
from http://nixos.org/nix

Once you have Nix installed, open a terminal in the top-level ML4PG directory
and run:

    nix-shell --pure

This will tell Nix to download and install all of ML4PG's dependencies, then
open a shell with only these dependencies available (omit "--pure" if you want
access to your regular commands too).

NOTE: These dependencies will *not* conflict with any existing software. They
will be installed into standalone directories and made available to the shell
via environment variables. The packages will also be cached, to prevent
downloading them every time. For more information, see the Nix Web site.

## Slow Start ##

For those who don't want to use Nix, or want to know a little about what it's
doing. These details are just an English prose version of `default.nix`.

### Dependencies ###

 - Emacs
 - ProofGeneral
 - Coq
 - Java runtime
 - Graphviz

ML4PG runs inside Emacs, on top of ProofGeneral. Although ProofGeneral works
with several languages/proof systems, ML4PG will only work with Coq.

The machine learning is performed by Weka, which is included, but Weka requires
a Java runtime to work.

Graphviz is used to generate the output.

### Environment ###

ML4PG can be controlled using several environment variables. If these aren't
provided, you will either be prompted to choose a value, or a default will be
used:

 - `ML4PG_HOME` is required, and should be the top-level directory of ML4PG,
   ending in a "/". This is used to load Emacs Lisp files and cache intermediate
   results. This will be set automatically by `nix-shell`.
 - `ML4PG_TYPE` should be either `coq` or `ssreflect`, and determines which
   implementation of ML4PG should be used. There is effort to bring these
   together into a common "generic" version, but that's not complete yet.
   Default is `coq`.
 - `HYPOTHESES_FILE`, if provided, should be a file name. During feature
   extraction, the hypotheses available at each proof step will be written to
   this file. Useful for distinguishing between tactic arguments and fresh
   variables.
 - `TEST_VERBOSE`, if provided and set to `t`, will output extra debug
   information. Mainly useful when running tests.
 - `TEST_SUITE` determines which test suite to run, either `coq` or
   `ssreflect`. Defaults to `ML4PG_TYPE`.
 - `ML4PG_TEST_PATTERN`, if provided, should be a regular expression matching
   the names of tests to run. All tests start with `ml4pg-`, so that's the
   default value.

## Running ML4PG ##

Once you have a shell with the dependencies available and ML4PG_HOME set
(whether by Nix or otherwise) you can start ML4PG. There are several ways to do
this:

### Testing ###

ML4PG comes with an automated test suite, which you can use to verify that it's
installed and working. The following command will run all tests:

    ./tests/runner.sh

### Interactive usage ###

It is recommended to run ML4PG in its own Emacs instance, since it might
interfere with existing buffers, or may cause Emacs to exit/crash.

You can launch Emacs with ML4PG available like this:

    emacs -l ml4pg.el

If you want to run ML4PG in an existing Emacs instance, you can use the
following snippet:

    (load-file "ml4pg.el")

Either way, you can then open a Coq file and ML4PG will provide a `Statistics`
menu of commands you can run.

You can also specify your Coq file via the commandline:

    emacs -l ml4pg.el --file my_script.v

### Non-interactive usage ###

ML4PG can be run non-interactively from the commandline or from a shell script.
To do this, use Emacs in "batch mode":

    emacs --batch -l ml4pg.el

That command will exit immediately after loading ML4PG; to do run something more
interesing you can provide some extra Emacs Lisp code in several ways. You can
provide an in-line snippet of ELisp on the command line:

    emacs --batch -l ml4pg.el --eval '(my-function)'

You can provide an Emacs Lisp file as a script (`--script` implies `--batch`):

    emacs -l ml4pg.el --script my-script.el

If your ELisp script loads ML4PG, via `(load-file "ml4pg.el")`, then you only
need to provide your script:

    emacs --script my-script.el

You can also give your script a "shebang" for executing Emacs automatically from
the shell, eg.

    $ cat my-script.el
    #!emacs --script
    (load-file "ml4pg.el")
    (your elisp goes here)

    $ ./my-script.el

## Extracting Hypotheses ##

When ML4PG's `(extract-feature-theorems)` function is called (bound to the menu
entry "Extract info up to point"), it will run through the Coq script and store
the names which are in-scope at each step.

If you want these to be saved to a file, you can provide a `HYPOTHESES_FILE`
environment variable, or set the LISP variable `hypotheses-file`.

These hypotheses are stored internally by the `proof-hypotheses` LISP variable.

For example, to extract the hypotheses from `foo.v`:

    $ ML4PG_MODE=coq HYPOTHESES_FILE=/tmp/foo.hyps emacs \
        --load ml4pg.el \
        --file foo.v    \
        --eval '(progn (goto-char (point-max)) (extract-feature-theorems))'
