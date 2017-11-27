#!/bin/bash
# See https://superuser.com/questions/363169/ps-how-can-i-recursively-get-all-child-process-for-a-given-pid

function append_child_pids {
  tp=`pgrep -aP $1`         # get childs pids of parent pid
  for i in $tp; do          # loop through childs
    if [ -z $i ]; then      # check if empty list
      exit                  # if empty: exit
    else                    # else
      echo "$i"             # print childs pid (one per line)
      append_child_pids $i  # call append_child_pids again with child pid as the parent
    fi;
  done
}

append_child_pids $1
