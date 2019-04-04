function syncthing-svali
    set host struna.me
    set remote_port 8384
    set local_port 8488
    ssh struna.me -L $local_port:localhost:$remote_port &
    open "http://localhost:$local_port"
    fg
end
