# Assets are compiled using command line tools

Package.json and bower.json control dependendcies for compilation. Lookup NPM or Bower package managers to find out more. 

From the /assets path, run:

```
$ npm install
$ bower install
```

Manifest.json controls the sources and output files for compilation. Gulpfile.json defines the tasks and tools used for compilation.

/assets/scripts and /assets/styles hold uncompiled JS and SCSS files. Make direct changes here, the from the /assets path, run:

```
$ gulp styles
```

or

```
$ gulp scripts
```

This will compile changes, show warnings for any errors/typos etc, and output minified assets to /assets/dist
