function fish_title
    if test $_ = "fish"
        prompt_pwd
    else
        echo $argv @ (prompt_pwd)
    end
end
