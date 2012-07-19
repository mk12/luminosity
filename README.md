Luminosity
==========

![Sample Luminosity render](https://github.com/mk12/Luminosity/blob/master/Sample.tga?raw=true)

Luminosity is a simple and elegant ray tracer written in Haskell.

Dependencies
------------

Before compiling, you need to get Luminosity's dependencies.

```sh
cabal install json --ghc-options=-DMAP_AS_DICT
cabal install blaze-builder
```

If you already have JSON, make sure to `--reinstall` with the `MAP_AS_DICT` option, otherwise Luminosity won't compile.

Compiling
---------

Compile Luminosity using the `build.sh` script. This will produce a `build` folder and a `luminosity` executable.

### Options

* `-t`: Compile with `-threaded` for parallelism
* `-p`: Compile with profiling enabled
* `-w`: Compile with `-Wall` (warnings)

If you want to compile it yourself, take a look at the end of [build.sh][] to see the command it uses.

[build.sh]: https://github.com/mk12/Luminosity/blob/master/build.sh

Running
-------

To render a scene, run the `luminosity` executable:

```sh
./luminosity Scene.json Image.tga
```

You can leave out the file extensions if you want. You can also leave out the second argument—in this case, the image would then be written to `Scene.tga`.

Scene Input
-----------

Rather than introducing some custom format, Luminosity uses JSON to describe a scene. JSON is easy to read and there are many existing validation tools.

To learn how to write scenes for Luminosity, check out the [Sample][] scene and the [Parse][] module.

Of special note, all colours and vectors are specified as 3-element arrays. Colour channels are floating-point (0.0–1.0, not 0–255). The width (and aspect ratio) of the camera is dependant on the resolution in the settings, while the height remains constant. `ortho-scale` is used to increase the size of an orthographic camera, and for perspective cameras, simply move it further back or adjust the `focal-length`.

[Sample]: https://github.com/mk12/Luminosity/blob/master/Sample.json
[Parse]: https://github.com/mk12/Luminosity/blob/master/Parse.hs

Image Output
------------

For now, the image output format is TGA (TARGA) for its simplicity. Soon, it will be PNG instead.

If you are rendering many images, I recommend using a tool such as ImageMagick's `convert` to convert them to PNG. This will save a lot of space, as the TGA files are not compressed.

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

Contribute
----------

Anyone is welcome to contribute! Please try to tackle one of the planned features listed above, and name your branch appropriately.

License
-------

Copyright © 2012 Mitchell Kember.

Luminosity is available under the MIT license, see [LICENSE][1] for details.

[1]: https://github.com/mk12/Luminosity/blob/master/LICENSE.md
