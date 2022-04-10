cat bios.sym  |sed 's/\.//g' | sed 's/ 00/ \$/g' | grep -v "@" | awk '{ print $3 " = " $2 } ' | sort | uniq | grep -v "__"
