#!/bin/bash
rm -r ../../help/*.txt
../../../wikitools/output/i386-linux/wikidownload --allpages="http://www.ullihome.de/index.php/Spezial:Allpages" --exportpage="http://www.ullihome.de/index.php/Spezial:Export" --imagedir="http://www.ullihome.de/images/" --pageoffset="Prometheus-Help" --output="../../help"
../../../wikitools/output/i386-linux/wiki2html "../../help"
rm -r ../../help/*.txt
./build_all_executables.sh gtk
sh build_rpm.sh gtk
sh build_deb.sh gtk
./build_all_executables.sh gtk2
sh build_deb.sh
sh build_rpm.sh
