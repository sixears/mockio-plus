#!/home/martyn/bin/bash

pkgs_gnugrep=$(dirname $( dirname $( realpath $(type -p grep) )))

for f in $( find proto/ -type f -name \*.hs ); do
  t=src/"${f#proto/}"
  perl -plE "s{__gnugrep__}{$pkgs_gnugrep}g" "$f" > "$t"
done
