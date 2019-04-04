function ipfs-share
    set -l hash_log (ipfs add $argv[1])
    set -l hash (echo $hash_log | awk '{ print $2; }')
    set -l iourl "https://ipfs.io/ipfs/$hash"
    echo $iorul | xclip -selection clipboard
    echo $iourl
end

