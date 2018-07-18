Modelling for the *Helicoverpa* project
============================================================================

This is the starting point for the *Helicoverpa* project. While I think that the workhorse of the model will likely need to be written in C, I am initialising this repository as an R package. This way, we can have a standalone version in C that is as fast as possible and can be run on a high performance cluster if need be, but also link the C code to R so that the model can be called from an R function (or shiny).

The src subdirectory will include all of the C code, and the R directory will include all of the R code. I use the notebook subdirectory to keep track of notes and scratch ideas; other directories are fine too, and we can use .Rignore to ignore them when building the R package. [Here](https://github.com/bradduthie/gmse) is a general idea of how I tend to organise projects. Shortly, I'll initialise a lab notebook like [the one for GMSE](https://bradduthie.github.io/gmse/notebook/gmse_notes.html) to keep a general timeline of the coding (of course, the commit history will do this too).


