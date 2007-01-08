#!/bin/awk -f
#
# buraq: this awk script was used to add a colon to .neg files in test suite.
#        though it might be useful to have it somewhere for non-awk users as a reference.
#
{ i = match ($0, /scala:[0-9]+/); 
   if(i == 0) {
	 print $0
   } else {
	   sub(/scala:[0-9]+/, "&:")
		   print $0
   }
}
