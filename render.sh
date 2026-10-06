#!/bin/sh
# Render the COBOL ray tracer to an mp4.
# Splits the timeline across all CPU cores (one raytrace process per chunk),
# then concatenates the raw RGB24 chunks and encodes with ffmpeg.
#
# usage: ./render.sh [WIDTH] [HEIGHT] [FRAMES] [OUT.mp4]
set -e
cd "$(dirname "$0")"
W=${1:-1920}
H=${2:-1080}
FRAMES=${3:-360}
OUT=${4:-cobol_raytracer.mp4}
JOBS=$(sysctl -n hw.ncpu 2>/dev/null || nproc)
TMP=/tmp/rt/render
mkdir -p "$TMP"

cobc -x -O2 -fno-binary-truncate -o raytrace raytrace.cob 2>/dev/null

PER=$(( (FRAMES + JOBS - 1) / JOBS ))
i=0
while [ $i -lt $JOBS ]; do
  first=$(( i * PER ))
  last=$(( first + PER - 1 ))
  [ $last -ge $FRAMES ] && last=$(( FRAMES - 1 ))
  if [ $first -le $last ]; then
    ./raytrace video "$W" "$H" $first $last > "$TMP/part_$i.rgb" &
  fi
  i=$(( i + 1 ))
done
wait

i=0
: > "$TMP/list.txt"
while [ $i -lt $JOBS ]; do
  [ -f "$TMP/part_$i.rgb" ] && echo "$TMP/part_$i.rgb" >> "$TMP/list.txt"
  i=$(( i + 1 ))
done
cat $(cat "$TMP/list.txt") | ffmpeg -v error -y -f rawvideo -pix_fmt rgb24 \
  -s "${W}x${H}" -r 30 -i - -c:v libx264 -pix_fmt yuv420p -crf 16 \
  -preset slow -movflags +faststart "$OUT"
rm -f "$TMP"/part_*.rgb
echo "wrote $OUT"
