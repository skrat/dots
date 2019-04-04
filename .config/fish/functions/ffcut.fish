function ffcut
	ffmpeg -i $argv[1] -ss $argv[2] -t $argv[3] -async 1 -c copy $argv[4]
end
