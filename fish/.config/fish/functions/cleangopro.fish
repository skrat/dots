function cleangopro
  set lrvs (for f in *.LRV
    if not test -e (echo $f | sed -e 's/\.LRV$/MP4/')
      echo $f
    end
  end)
  set thms (for f in *.THM
    if not test -e (echo $f | sed -e 's/\.THM/MP4|JPG/')
      echo $f
    end
  end)
  set files $lrvs $thms
  set files_len (string length $files; or echo 1)
  if math "$files_len > 1" > /dev/null
    echo "Delete following files?"
    echo $files
    read confirm -P "y/n " -n 1
    if [ $confirm = "y" ]
      rm $files
    end
  end
end
