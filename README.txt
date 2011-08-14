gloss-web -- Web-based front-end to Ben Lippmeier's gloss package

With gloss-web, you can host an online development environment capable of
building applications in the gloss graphics library with Haskell!

WARNING! WARNING! WARNING!
--------------------------

This is a very early release of the project, and it's currently incredibly
dangerous to run in any kind of public setting.  By design, this application
allows execution of arbitrary code on your computer with no security at all!

* Do NOT run the server on a computer with sensitive information on it like
  private SSH keys or personal data.

* Do NOT run the server on a public network.

* Be 100% CERTAIN of your firewall configuration before running.

* Do NOT tell any Haskellers with a malfunctioning sense of humor that you're
  running the server, or where.

In the future, the idea is to use GHC 7.2's SafeHaskell extension to check that
the code doesn't get to run I/O actions, and to add resource limits and other
safety mechanisms... but THAT IS NOT DONE YET!

How To Run:
-----------

If you've read and understood the warning above, here's how to get started.

1. Make sure you've got the Haskell Platform installed (any recent version).
2. Check out this project into a directory.
3. Change to that directory and type 'cabal install'.
4. When the build finishes, type "gloss-web" to run the server.

The default port is 8000, so you can now visit 'http://localhost:8000' to
access the server.

You MUST run the server from the project directory.

How to Use:
-----------

1. Visit the URL for the server (if it's running locally and you didn't pass
   any command line parameters, that's http://localhost:8000)
2. Enter your code in the editor you see on that page.
3. Click "Run" to see the resulting image.

If you don't see the image load when you click "Run", make sure you enable
JavaScript and disable any popup blockers.

Your source code stored in a cookie in your browser, so you'll be able to
quit, and go back later to finish editing.  That said, though, cookies tend
to get deleted sometimes, so you probably want to keep another copy
elsewhere!  (It's also stored on the server in the tmp directory, but you
can't get it from there through the web interface yet.)

How to Help:
------------

This is a bare skeleton of the project.  It needs a lot of work, including:

1. Testing and fixing in more browsers.
2. Some better way to load images, given that you can't use the IO monad.
3. Use of SafeHaskell to control execution.
4. Resource, time, and other limits as a safeguard against DOSing the server.
5. Support for the animation, simulation, and game interfaces of gloss.
6. Better organization of the code... I'm holding off for Snap 0.6 before
   taking that seriously, though.

The latter is a tough job, since what we really want is real-time streaming
of the pictures, and not polling in new requests.  Comet is notoriously
difficult.

I plan to tackle all of this stuff in time... but if you want it sooner, I'm
happy to accept patches.
