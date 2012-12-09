Luminosity
==========

Luminosity is a simple and elegant ray tracer written in Haskell.

![Sample Luminosity render](http://i.imgur.com/y1a0A.png)

Dependencies
------------

Before compiling, get Luminosity's dependencies.

    $ cabal install json --ghc-options=-DMAP_AS_DICT
    $ cabal install blaze-builder

*Add* `--enable-documentation` *if you want their symbols to be linked when generating Luminosity's documentation.*

If you already have JSON, make sure to `--reinstall` with the `MAP_AS_DICT` option, otherwise Luminosity won't compile.

Compiling
---------

    $ cabal build

The executable will be located in `dist/luminosity`.

    $ cabal install

This will install Luminosity to your user's local cabal directory.

Documentation
-------------

    $ cabal haddock --executables --hyperlink-source

Add `--haddock-option=-ignore-all-exports` to see documentation for all functions.

Running
-------

    $ luminosity Scene.json Image.tga

You can leave out the second argument (in this case the image would be written to `Scene.tga`).

Scene Input
-----------

Rather than introducing some custom format, Luminosity uses JSON to describe a scene. JSON is easy to read and there are many existing validation tools.

To learn how to write scenes for Luminosity, check out the [Sample][] scene and the [Parse][] module.

Of special note, all colours and vectors are specified as 3-element arrays. Colour channels are floating-point (0.0–1.0, not 0–255). The width (and aspect ratio) of the camera is dependant on the resolution in the settings, while the height remains constant. Use `ortho-scale` to increase the size of an orthographic camera, and for perspective cameras, simply move it further back or adjust the `focal-length`.

[Sample]: https://github.com/mk12/Luminosity/blob/master/Sample.json
[Parse]: https://github.com/mk12/Luminosity/blob/master/src/Luminosity/Parse.hs

Image Output
------------

For now, the image output format is uncompressed TGA (Targa) for its simplicity. Soon, it will be PNG instead.

Features
--------

### Current Features

* JSON scene input
* TGA image output
* sRGB conversion
* Orthographic and perspective projections
* Sphere and plane primitives
* Lambert diffuse reflection
* Ray traced reflection

### Planned Features

* PNG image output
* Parallel rendering
* Faster rendering (optimized `Vector`)
* Anti-aliasing (supersampling)
* Exposure operator (rather than saturation)
* Blinn–Phong reflection
* Transparency and refraction
* Fresnel effect
* More `Surface`s (Cubes, Cones, Cylinders, Metaballs)
* Directional light sources
* Depth of field
* Procedural and bitmap textures
* Bump mapping

Contributing
------------

Anyone is welcome to contribute!

License
-------

Copyright © 2012 Mitchell Kember

Luminosity is available under the MIT license; see [LICENSE][] for details.

[LICENSE]: https://github.com/mk12/luminosity/blob/master/LICENSE.md
