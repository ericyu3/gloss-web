gloss-web -- Web-based front-end to Ben Lippmeier's gloss package

With gloss-web, you can host an online development environment capable of
building applications in the gloss graphics library with Haskell!

--------

WARNING: This application runs user-uploaded code on your server.  It uses the
SafeHaskell extension and only evaluates pure values, so it ought to be safe.
However, SafeHaskell was released 6 days ago, so it's up to you to decide
whether you trust it or not.

In addition, there's currently no protection against:

* Eating up 100% of your CPU time.
* Using bandwidth to transfer infinite-sized results.
* Crashing the server by using unbounded amounts of memory.

Because of this, it's probably not appropriate for running on a shared server
with other important applications.

---------------
How To Install:
---------------

Sadly, installation has gotten a touch more difficult.  You'll first need a
modified version of the gloss library.

1. darcs get http://code.ouroborus.net/gloss/gloss-head/
2. Change directories into the gloss-head directory
3. Edit library/gloss.cabal and add the following lines right after the
   existing ghc-options line:

    If impl(ghc >= 7.2)
      ghc-options: -XTrustworthy

4. Type 'cabal install' and wait for the install to complete.
5. Type 'ghc-pkg trust base' to mark base as a trusted package.
6. Type 'ghc-pkg trust gloss' to mark gloss as a trusted package.

At this point, you'll have a "trustworthy" version of gloss installed, and have
marked base as "trusted".  Note that this has security implications in case a
security vulnerability should be discovered in gloss or base, so technically
speaking you ought to vet the gloss source code to convince yourself it's okay.
We'll wait for you to do that... okay, welcome back.  Time to install the
server.

1. Check out this project into a directory.
2. Change to that directory and type 'cabal install'.
3. When the build finishes, type "gloss-web" to run the server.

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
3. Think about and work out the best way to apply SafeHaskell.
4. Resource, time, and other limits as a safeguard against DOSing the server.
5. Better organization of the code... I'm holding off for Snap 0.6 before
   taking that seriously, though.
6. Support for the animation, simulation, and game interfaces of gloss.

The last is a tough job, since what we really want is real-time streaming
of the pictures, and not polling in new requests.  Comet is notoriously
difficult.

I plan to tackle all of this stuff in time... but if you want it sooner, I'm
happy to accept patches.
