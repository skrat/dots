function ffrec
	slop -f "%x %y %w %h" | read X Y W H
	# ffmpeg -f x11grab -r 25 -s "$W"x"$H" -i :0.0+$X,$Y $HOME/screen.webm
	set width (math $W + $W \% 2)
	set height (math $H + $H \% 2)
	ffmpeg -y -f x11grab -r 30 -s "$width"x"$height" -i :0.0+$X,$Y -c:v libx264 -pix_fmt yuv420p -qp 0 -preset ultrafast -crf 35 -threads 4 screen.mp4
end
