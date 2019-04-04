function spin -d "Background job spinner"
  set -l format " @ \r"
  set -l commands $argv
  set -l spinners '\|/-'
  set -l error /dev/stderr

  set spinners (printf "%s\n" "$spinners" | grep -o .)

  set -l tmp (mktemp -t spin.XXX)
  set -l job_id

  fish -c "$commands" > /dev/stdout ^ $tmp &

  set job_id (jobs -l | awk -v FS=\t '
        /[0-9]+\t/{
            jobs[++job_count] = $1
        }
        END {
            for (i = 1; i <= job_count; i++) {
                print(jobs[i])
            }
            exit job_count == 0
        }
  ')

  while contains -- $job_id (jobs | cut -d\t -f1 ^ /dev/null)
    if status --is-interactive
      for i in $spinners
        printf "$format" | awk -v i=(printf "%s\n" $i | sed 's/=/\\\=/') '
                {
                    gsub("@", i)
                    printf("%s", $0)
                }
                ' > /dev/stderr
        sleep 0.05
      end
    end
  end

  if test -s $tmp
    command cat $tmp > $error
    command rm -f $tmp
    return 1
  end

  command rm -f $tmp
end

function superes
  set -l filter   $argv[1]
  set -l stack    $argv[2]
  set -l files    $argv[3..-1]
  set -l upgeom   "200%"
  set -l downgeom "70%"
  #if test -e (dirname $files[1])/dark.tif
  #    echo "# Subtracting dark frame..."
  #    set -l dark true
  #    bar -c "convert \$bar_file -colorspace rgb -compose subtract dark.tif -colorspace sRGB sub_\$bar_file" $files
  #end
  echo "# Aligning..."
  spin "align_image_stack -a al_ -t 0.3 -c 20 $files"
  echo "# Resizing by $upgeom..."
  bar -c "convert \$bar_file -colorspace rgb -resize $upgeom -filter $filter -colorspace sRGB up_\$bar_file" al_*
  echo "# Stacking..."
  spin "convert up* -evaluate-sequence $stack stacked.tif"
  echo "# Shrinking by $downgeom..."
  spin "convert stacked.tif -colorspace rgb -resize $downgeom -filter $filter -colorspace sRGB final.tif"
  echo "# Cleaning up..."
  rm al_* up_* stacked.tif
  echo "# Output written to final.tif"
end
