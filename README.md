# COBOL ray tracer

A ray tracer written in COBOL. It renders live in a terminal at about 18 fps, or writes 1080p frames for ffmpeg.

![demo](docs/demo.gif)

Four spheres orbit over a checkerboard floor. Each pixel is a traced ray with up to five mirror bounces, a shadow ray toward the sun, and Fresnel reflections on the floor. The whole thing is one file, `raytrace.cob`, built with GnuCOBOL 3.x and no C code.

## Run it

```sh
cobc -x -O2 -fno-binary-truncate -o raytrace raytrace.cob
./raytrace live 140 45 120      # COLS ROWS SECONDS
./render.sh                     # 1920x1080, 360 frames (12 s), all cores
```

Live mode needs a truecolor terminal. `render.sh` needs ffmpeg.

## Speed

GnuCOBOL runs most arithmetic through a decimal library. On an M1, a floating-point dot product takes about 1.5 µs and `FUNCTION SQRT` about 1.7 µs, while the same dot product on binary integers takes 0.1 µs. So the per-pixel math is fixed point (4096 means 1.0), square roots come from a lookup table, and camera rays are normalized once at startup. Sphere hits use the stable discriminant from *Ray Tracing Gems*, because the textbook `b² − c` loses enough precision in fixed point to make sphere edges fuzzy.

## Prior art

I couldn't find an earlier ray tracer in COBOL. The closest is [FPS.cob](https://github.com/icitry/FPS.cob), a raycasting shooter.

## License

MIT
