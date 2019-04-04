function disowned
	for f in (find $argv)
		if not pacman -Qo $f > /dev/null ^&1
			echo $f
		end
	end
end
