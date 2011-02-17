Introduction
============

In Javascript environments where a Firebug like tool is not available,
repl-server can be used to get a REPL. It requires two things from the
Javascript environment:

* An HTTP client like XMLHttpRequest
* A JSON parser/serializer

It should work with any modern browser, and it also works with
Titanium Mobile applications. It can also work with any other
Javascript environment, provided the above two requirements are met.

repl-server has two components:

* A CL based (web) server, which also provides the REPL.
* A javascript file (repl-client.js) which needs to be included in the
  application

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

In the repl-server directory, fire up your Common Lisp compiler:

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

To use the javascript client, copy `repl-client.js` into your
web/javascript application. After it is loaded, you can start the
ReplClient by passing it the URL of your REPL server:

    var client = new ReplClient('http://localhost:8000');

To stop communicating with the REPL server, call `stop`:

    client.stop();
