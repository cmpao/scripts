#!/bin/bash

inotifywait -m -r --format "%e %w%f"  ${PWD} -e create -e moved_to -e moved_from -e delete |
while read action file; do
   echo "$action $file"
   if [ "$action" = "DELETE,ISDIR" ]
   then
       echo ""
   fi
done
