Introduction
============

In Javascript environments where a Firebug like tool is not available,
repl-server can be used to get a REPL. It requires two things from the
Javascript environment:

* An HTTP client like XMLHttpRequest
* A JSON parser/serializer

It should work with any modern (web or mobile) browser, and it also
works with Titanium Mobile and Phonegap applications. It can also work
with any other Javascript environment, provided the above two
requirements are met.

repl-server has two components:

* A CL based (web) server, which also provides the REPL.
* A javascript file (repl-client.js) which needs to be included in the
  application

Features include:

* Syntax checker, thanks to
  [parse-js](http://marijnhaverbeke.nl/parse-js/)
* Colored output on ANSI terminals
* Special variables like `_` for quick access to previously returned
  values
* Load javascript code from local files using the `//load` command

The features section below explain some of these in more detail.

Download
========

Check out the source code from github:

    git clone git://github.com/chaitanyagupta/repl-server.git

Requirements
============

Common Lisp
-----------

Setting up a Common Lisp environment for repl-server requires three
things:

1. A CL compiler. e.g. [SBCL](http://www.sbcl.org/) (Linux) or
   [Clozure CL](http://ccl.clozure.com/) (OS X or Windows).

2. [Quicklisp](http://www.quicklisp.org/beta/) is required to download
   Common Lisp libraries.

3. Finally, there are the libraries which repl-server
   requires. Quicklisp will download them automatically the first time
   you load repl-server.

Getting Started
===============

Running the server
------------------

The quick and easy way to run the repl-server is to load
`example-launch.lisp` in your Lisp runtime. Change to the repl-server
directory and run the following command:

    $ sbcl --load example-launch.lisp

Clozure CL also supports a --load option. To run CCL, run:

    $ ccl --load example-launch.lisp

If everything goes well, you should see a `REPL>` prompt on your
terminal.

Here's what you should do in case you didn't use `example-launch.lisp`.

In the repl-server directory, fire up your Common Lisp runtime:

    $ sbcl

Once you have the CL REPL, load repl-server with this command:

    (ql:quickload :repl-server)

The first time you do this, Quicklisp will ask your permission to
download various Common Lisp libraries which are required to run
repl-server.

Once the required libraries are downloaded, and the repl-server source
code is loaded, you start the HTTP server:

    (repl-server:start-server)

By default, the server listens on port 8000. To make it listen on
another port, you can pass the port number:

    (repl-server:start-server 9000)

Now, start the javascript REPL:

    (repl-server:start-repl)

Installing the javascript client
--------------------------------

Copy `repl-client.js` into your web/javascript application. After it
is loaded, you can start the ReplClient by passing it the URL of your
REPL server:

    ReplClient.run('http://localhost:8000');

To stop communicating with the REPL server, call `stop`:

    ReplClient.stop();

Features
========

Special variables
-----------------

Inspired by the Common Lisp variable `*` and friends, repl-server
defines a few variables which provide similar functionality in
Javascript. Specifically, the following variables contain:

* `_`: the value returned by the last expression evaluated at the REPL
* `__`: the value of the second last expression evaluated at the REPL
* `___`: the value of the third last expression evaluated at the REPL

* `_$`: the last expression evaluated at the REPL (returned as a string)
* `_$$`: the second last expression evaluated at the REPL
* `_$$$`: the third last expression evaluated at the REPL

For example, if the last three forms and their results at the repl
look like this:

    REPL> x = "foo";
    "foo"

    REPL> y = "bar";
    "bar"

    REPL> x + y;
    "foobar"

Then the values returned by the following forms (entered one after the
other) will look like this:

    REPL> [_, __, ___];
    ["foobar","bar","foo"]

    REPL> [_$, _$$, _$$$];
    ["[_, __, ___];","x + y;","y = \"bar\";"]

Commands
--------

Commands are special forms (which you type at the REPL) which are
treated differently from Javascript forms. A command starts with
`//`. This is followed immediatedly by the command name. A command can
have one or more arguments. Arguments are delimited by
whitespace. They can also be delimited by double quotes in case you
want to pass an argument containing whitespace(s).

    //command arg1 arg2 ...

Currently, the following commands are defined:

* `//quit`: Quit the current REPL

* `//version`: Returns the server version. Note: if the version of
  repl-client.js differs from that of the server, repl-server gives a
  warning when a client is connected.

* `//load`: Takes one argument -- the path to a .js file on the local
  machine. This contents of the entire file are eval'd.

* `//pwd`: Returns the present working directory. This gives the
  default directory for the `//load` command.

* `//cd`: Takes one argument which should be a directory. This will
  updated your pwd.

Colors
------

On ANSI terminals, repl-server can use colors to differentiate between
different display elements. You can define colors and styles for the
following:

* The REPL prompt (`:repl`)
* The value returned by evaluating a form (`:result`)
* Informational messages (`:info`)
* Warnings (`:warn`)
* Errors (`:error`)

You can define these colors using the Lisp macro
`repl-server:defcolor`. Evaluate `(repl-server:define-color type ...)`
before starting the REPL. Some examples:

    (repl-server:defcolor :repl :style :bright)
    (repl-server:defcolor :result :fg :white)
    (repl-server:defcolor :info :bg :cyan)
    (repl-server:defcolor :warn :fg :yellow)
    (repl-server:defcolor :error :style :bright :fg :red)

In the above forms, `:fg` stands for foreground and `:bg` stands for
background. The available colors are:

* `:BLACK`
* `:RED  `
* `:GREEN`
* `:YELLO`
* `:BLUE `
* `:VIOLE`
* `:CYAN `
* `:WHITE`

The available styles are:

* `:BRIGHT`
* `:DIM`
* `:UNDERLINE`
* `:NEGATIVE`
* `:CONCEAL`
* `:CROSS-OUT`
* `:NORMAL`
* `:NO-UNDERLINE`
* `:POSITIVE`
* `:REVEAL`
* `:NO-CROSS-OUT`
