#!/bin/bash
Widgetset=$1
if [ "x$Widgetset" = "x" ]; then
  Widgetset=gtk2
fi
echo "compiling for $1..."
rm ../../output/i386-linux/cdmenue
lazbuild -B --widgetset=$1 ../../src/cdmenue.lpi
rm ../../output/i386-linux/pstarter
lazbuild -B --widgetset=$1 ../../src/pstarter.lpi
rm ../../output/i386-linux/updatedatabase
lazbuild -B --widgetset=$1 ../../src/updatedatabase.lpi
rm ../../output/i386-linux/dbaserepair
lazbuild -B --widgetset=$1 ../../src/dbaserepair.lpi
rm ../../output/i386-linux/dbsync
lazbuild -B --widgetset=$1 ../../src/dbsync.lpi
rm ../../output/i386-linux/prometheus
lazbuild -B --widgetset=$1 ../../src/prometheus.lpi
cp ../../output/i386-linux/pstarter ../executables/
