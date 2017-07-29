#!/usr/bin/env bash
set -eu -o pipefail

# echo "bash cutter.sh rn33.mp4"

# From: https://stackoverflow.com/questions/45304233/execute-command-in-bash-script-until-output-exceeds-certain-value
# test -f stack_overflow_q45304233.tar ||  curl -k -O https://84.19.186.119/stack_overflow_q45304233.tar
# test -f stack_overflow_q45304233.tar ||  curl -k -O https://84.19.186.119/stack_overflow_q45304233.tar
# test -f rn33.mp4 || curl -k -O https://84.19.186.119/rn33.mp4

function parser () {
  local max="$1"
  local max_int

  # NB: this removes everything after the decimal point
  max_int="${max%.*}"

  # I added a line number so I could match up the ouptut from this function
  # with the output captured by the 'tee' command
  local lnum="0"
  while read -r tc;
    do

      lnum="$(( 1 + lnum ))"

      # if a blank line is read, just ignore it and continue
     if [ -z "$tc" ]; then
       continue
     fi

     local tc_int
     # NB: this removes everything after the decimal point
     tc_int="${tc%.*}"
     echo "Read[$lnum]: $tc"

     if (( "$tc_int" >= "$max_int" )); then
       echo "Over 30: $tc";
       # This closes stdin on this process, which will cause an EOF on the
       # process writing to us across the pipe
       exec 0>&-
       return 0
     fi

    done
}

# echo "bash version:    $BASH_VERSION"
# echo "ffprobe version: $(ffprobe -version | head -n1)"
# echo "sed version:     $(sed --version | head -n1)"

# NB: by adding in the 'tee ffprobe.out' into the pipeline I was able to see
# that it was producing lines like:
#
# 0|28.520000
# 1|28.560000
#
#
# changing the sed to look for any single digit and a pipe fixed the script
# another option is to use cut, see below, which is probalby more robust.

# ffprobe "$1" \
# 	-hide_banner \
# 	-select_streams v \
# 	-show_entries frame=key_frame,best_effort_timestamp_time \
# 	-of csv=nk=1:p=0:s="|" \
# 	-v quiet 2>&1 | \
#   tee ffprobe.out |
# 	sed -ne "s/^[0-9]|//p" | \
# 	parser 30


ffprobe "$1" \
	-hide_banner \
	-select_streams v \
	-show_entries frame=key_frame,best_effort_timestamp_time \
	-of csv=nk=1:p=0:s="|" \
	-v quiet 2>&1 | \
	cut -f2 -d\| | \
	parser 30

