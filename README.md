# Luminosity

Luminosity is a simple and elegant ray tracer written in Haskell.

![Sample Luminosity render](http://i.imgur.com/y1a0A.png)

## Dependencies

Before compiling, get Luminosity's dependencies.

	$ cabal install json --ghc-options=-DMAP_AS_DICT
	$ cabal install blaze-builder

_Add_ `--enable-documentation` _if you want their symbols to be linked when generating Luminosity's documentation._

If you already have JSON, make sure to `--reinstall` with the `MAP_AS_DICT` option, otherwise Luminosity won't compile.

## Compiling

	$ cabal build

The executable will be located in `dist/luminosity`.

	$ cabal install

This will install Luminosity to your user's local cabal directory.

## Documentation

	$ cabal haddock --executables --hyperlink-source

Add `--haddock-option=-ignore-all-exports` to see documentation for all functions.

## Running

	$ luminosity Scene.json Image.tga

You can leave out the second argument (in this case the image would be written to `Scene.tga`).

## Scene input

Rather than introducing some custom format, Luminosity uses JSON to describe a scene. JSON is easy to read and there are many existing validation tools.

To learn how to write scenes for Luminosity, check out the [Sample][] scene and the [Parse][] module.

Of special note, all colours and vectors are specified as 3-element arrays. Colour channels are floating-point (0.0–1.0, not 0–255). The width (and aspect ratio) of the camera is dependant on the resolution in the settings, while the height remains constant. Use `ortho-scale` to increase the size of an orthographic camera, and for perspective cameras, simply move it further back or adjust the `focal-length`.

[Sample]: Sample.json
[Parse]: src/Luminosity/Parse.hs

## Image output

For now, the image output format is uncompressed TGA (Targa) for its simplicity. Soon, it will be PNG instead.

## Features

### Current features

- JSON scene input
- TGA image output
- sRGB conversion
- orthographic and perspective projections
- sphere and plane primitives
- Lambert diffuse reflection
- ray traced reflection

### Planned features

- PNG image output
- parallel rendering
- faster rendering (optimized `Vector`)
- anti-aliasing (supersampling)
- exposure operator (rather than saturation)
- Blinn–Phong reflection
- transparency and refraction
- Fresnel effect
- more `Surface`s (Cubes, Cones, Cylinders, Metaballs)
- directional light sources
- depth of field
- procedural and bitmap textures
- bump mapping

## License

© 2012 Mitchell Kember

Luminosity is available under the MIT License; see [LICENSE](LICENSE.md) for details.
