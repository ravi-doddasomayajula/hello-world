#!/bin/csh

if( ${#argv} < 2 ) then
  echo "usage: findid aneuid table1 [... tableN]"
  exit
endif
set ids = "$1"

if( -e $ids ) then
  set lid = `cat $ids`
else
  set lid = "$ids"
endif

foreach aid ($lid)
  @ i = 1
  foreach tab ($argv)
    if( $i > 1 ) then
      awk -v id=$aid '{if(id~$1 && length(id)==length($1))print;}' $tab
    endif
    @ i = $i + 1
  end
end
