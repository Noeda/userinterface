This is an experimental UI library for Haskell based on Qt 5. The eventual goal
is to be able to build native-looking user interfaces entirely from Haskell,
perhaps declaratively, with proper garbage collection. We'll see.

Right now it binds some Qt 5 classes so you can make an application window,
text edits and stuff. Everything is garbage collected so you don't have to mess
with any low-level details (unless you want to; this library will let you).

At the moment there is a good chance it breaks and cracks for you.

You need a C++ compiler, qmake and make in PATH for this library to
successfully compile.

