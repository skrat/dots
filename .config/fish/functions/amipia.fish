function amipia
	set -l norm (spin "curl -s https://www.privateinternetaccess.com/ | hxnormalize -x 2>/dev/null")
	set -l ISP (echo $norm | hxselect ".topbar__item:nth-child(2)" | w3m -dump -cols 2000 -T 'text/html')
	set -l stat (echo $norm | hxselect ".topbar__item:nth-child(3)" | w3m -dump -cols 2000 -T 'text/html')
	if echo $stat | grep not > /dev/null
		set_color --bold red
	else
		set_color --bold green
	end
	echo $stat
	set_color normal
	echo $ISP
end
