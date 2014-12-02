uld-hd
======

A port of Werks ULD library for OpenGL (WebGL + fragment shader)

Video demonstrating one screensaver-ish animation (unfortunately, low quality - the real thing is smooth):

https://www.youtube.com/watch?v=_VKXYy0L1Vg&feature=youtu.be


Try this from the online in-browser cloud9 IDE!
===============================================

- Go to https://c9.io/ and sign up/in. You can sign in with your GitHub account.
- Create new workspace -> Clone from URL: `git@github.com:Ahnfelt/uld-hd.git`
- Open the workspace for editing.
- Click on the "bash" tab in the bottom of the window and run these commands:

``cabal update``

``cabal install mtl``

- To generate your first shader, run:

``runhaskell Examples/TimeLens.hs``
  
- This will generate an `TimeLens.hs.html` file. Right click on this file and select "preview". If you have WebGL enabled, this will show an animation (otherwise you'll get a blank page).

When you want to try another example, simply `runhaskell Examples/SomethingFooBar.hs` and then click the refresh icon on the tab that displays the animation.
