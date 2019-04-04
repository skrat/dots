function mp42gif
  ffmpeg -i "$1" -vf "scale=trunc(iw/2):trunc(ih/2)" -r 10 -f image2pipe -vcodec ppm - | convert -delay 5 -loop 0 - "$1.gif"
end
