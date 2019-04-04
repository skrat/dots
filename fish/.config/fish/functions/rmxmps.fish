function rmxmps
	for f in *.xmp
		if not test -e (echo $f | sed -e 's/\.xmp$//')
			echo $f
        end
    end | xargs rm
end

