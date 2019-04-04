function google
	set q (python -c "import urllib.parse, sys; print(urllib.parse.quote_plus(sys.argv[1]))" "$argv")
	xdg-open "http://www.google.com/search?q=$q"
end

