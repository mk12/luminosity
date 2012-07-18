Luminosity
==========

![Sample Luminosity render](Sample.tga)

Luminosity is a simple and elegant ray tracer written in Haskell.

Compiling
---------

Compile Luminosity using the `build.sh` script. This will produce a `build` folder and a `luminosity` executable.

### Options

* `-t`: Compile with `-threaded` for parallelism
* `-p`: Compile with profiling enabled
* `-w`: Compile with `-Wall` (warnings)

If you want to compile it yourself, take a look inside [build.sh][] to see the command it uses.

[build.sh]: build.sh

Running
-------

Rather than introducing some custom format, Luminosity uses JSON to describe a scene. The output format is TGA (TARGA) for its simplicity, but when I get around to implementing it, Luminosity will only output PNG files.

To render a scene, run the `luminosity` executable with:

```sh
./luminosity Scene.json Image.tga
```

You can leave out the file extensions if you wish. You can also leave out the second argument—in this case, the image would then be called `Scene.tga`.

To learn more about the JSON structure for Luminosity scenes, check out the [Sample][] scene and the [Parse][] module. 

[Sample]: Sample.json
[Parse]: Parse.hs

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
* Transparency + refraction
* Fresnel effect
* More `Surface`s (Cubes, Cones, Cylinders, Metaballs)
* Directional light sources
* Depth of field
* Procedural and bitmap textures

Contribute
----------

Anyone is welcome to contribute! Please try to tackle one of the planned features listed above, and name your branch appropriately.

License
-------

Copyright © 2012 Mitchell Kember.

Luminosity is available under the MIT license, see [LICENSE][1] for details.

[1]: LICENSE.md
