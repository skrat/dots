# name: eclm
function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _is_git_dirty
  echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function _git_stats
  set -l gss (git diff --shortstat)
  if [ $gss ]
    echo (set_color -o red)-(echo $gss | cut -d' ' -f7)(set_color -o green)+(echo $gss | cut -d' ' -f5)
  else
    echo (set_color -o green)"*"(git status --porcelain 2>/dev/null| grep "^??" | wc -l)
  end
end

function fish_prompt
  set -l last_status $status
  set -l cyan (set_color -o cyan)
  set -l magenta (set_color -o magenta)
  set -l yellow (set_color -o yellow)
  set -l red (set_color -o red)
  set -l blue (set_color -o blue)
  set -l green (set_color -o green)
  set -l normal (set_color normal)

  set arrow "$green➜ "

  if test $last_status = 0
      set status_indicator "$green "
  else
      set status_indicator "$red!"
  end

  set -l cwd $cyan(prompt_pwd)

  if [ (_git_branch_name) ]
    if test (_git_branch_name) = 'master'
      set -l git_branch (_git_branch_name)
      set git_info (set_color -o yellow)mstr
    else
      set -l git_branch (_git_branch_name)
      set git_info "$blue$git_branch"
    end

    if [ (_is_git_dirty) ]
      set -l dirty (_git_stats)  # "$yellow ✗"
      set git_info "$git_info$yellow/$dirty"
    end

    set git_info "$blue ($git_info$blue)"
  end

  if set -q VIRTUAL_ENV
    set -l _vf_info (basename "$VIRTUAL_ENV")
    set vf_info "$yellow [$_vf_info]"
  end

  echo -n -s $status_indicator $arrow $cwd $vf_info $git_info $normal ' '
end

function fish_right_prompt
  set_color blue
  if [ (hostname) = "apex" ]
    set xhostname ""
  else
    set xhostname "$USER@"(hostname)
  end
  echo -n -s $xhostname
end
