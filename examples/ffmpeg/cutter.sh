#!/usr/bin/env bash
set -eu -o pipefail

test -f stack_overflow_q45304233.tar ||  curl -k -O https://84.19.186.119/stack_overflow_q45304233.tar

function parser () {
  local max="$1"
  local max_int

  # NB: this removes everything after the decimal point
  max_int="${max%.*}"

  while read tc;
    do

      # if a blank line is read, just ignore it and continue
     if [ -z "$tc" ]; then
       continue
     fi

     local tc_int
     # NB: this removes everything after the decimal point
     tc_int="${tc%.*}"
     echo "Read: $tc"

     if (( "$tc_int" >= "$max_int" )); then
       echo "Over 30: $tc";
       # This closes stdin on this process, which will cause an EOF on the
       # process writing to us across the pipe
       exec 0>&-
       return 0
     fi

    done
}

echo "bash version:    $BASH_VERSION"
echo "ffprobe version: $(ffprobe -version | head -n1)"
echo "sed version:     $(sed --version | head -n1)"

ffprobe "$1" \
	-hide_banner \
	-select_streams v \
	-show_entries frame=key_frame,best_effort_timestamp_time \
	-of csv=nk=1:p=0:s="|" \
	-v quiet | \
	sed -ne "s/^1|//p" | \
	parser 30

