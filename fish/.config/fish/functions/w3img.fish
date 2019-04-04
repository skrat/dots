function w3img
  test -f "$1"; and exit

  set W3MIMGDISPLAY "/usr/lib/w3m/w3mimgdisplay"
  set FILENAME $1
  set FONTH 14 # Size of one terminal row
  set FONTW 8  # Size of one terminal column
  set COLUMNS (tput cols)
  set LINES (tput lines)

  echo "
  import array, fcntl, termios
  buf = array.array('H', [0, 0, 0, 0])
  fcntl.ioctl(1, termios.TIOCGWINSZ, buf)
  print(buf[2], buf[3])
  " | python

  # echo -e "5;$FILENAME" | $W3MIMGDISPLAY | read width height

  # set -l max_width (math "$FONTW * $COLUMNS")
  # set -l max_height $(($FONTH * $(($LINES - 2)))) # substract one line for prompt

  # if test $width -gt $max_width; then
  #   height=$(($height * $max_width / $width))
  #   width=$max_width
  #   fi

  #   if test $height -gt $max_height; then
  #     width=$(($width * $max_height / $height))
  #     height=$max_height
  #     fi

  #     w3m_command="0;1;0;0;$width;$height;;;;;$FILENAME\n4;\n3;"

  #     tput cup $(($height/$FONTH)) 0
  #     echo -e $w3m_command|$W3MIMGDISPLAY

  #   end
end
