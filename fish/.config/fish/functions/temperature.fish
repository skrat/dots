# Defined in /home/skrat/.config/fish/functions/temperature.fish @ line 1
function temperature
    set -l ip (curl -s ipinfo.io/ip)
    set -l lookout (runcached.py -c 10800 curl -s https://www.iplocate.io/api/lookup/$ip)
    set -l city (echo $lookout | jq -r ".city")
    set -l lat (echo $lookout | jq ".latitude")
    set -l lon (echo $lookout | jq ".longitude")
    printf "$city "
    curl -s "https://api.met.no/weatherapi/locationforecast/1.9?lat=$lat&lon=$lon" | \
    xmllint --xpath "/weatherdata/product/*[1]/location/temperature/@value" - | \
    cut -d\" -f2
end
