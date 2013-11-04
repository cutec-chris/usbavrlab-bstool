#!/bin/bash
Program=prometheus
Widgetset=$1
if [ "x$Widgetset" = "x" ]; then
  Widgetset=gtk2
fi
Arch=$(fpc -v | grep 'Compiler version' | sed 's/.*for \([^ ]\+\)$/\1/')
# get date of day
Year=`date +%y`
Month=`date +%m`
Day=`date +%d`
Date=20$Year$Month$Day
TmpDir=/tmp
BuildDir=$TmpDir/software_build
Version=$(cat ../../src/version.inc).$(cat ../../src/revision.inc)
echo "Build directory is $BuildDir"
if [ x$BuildDir = x/ ]; then
  echo "ERROR: invalid build directory"
  exit
fi
rm -rf $BuildDir
echo "creating spec file..."
mkdir -p $BuildDir/RPM
cat rpm/$Program.spec.template | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
  > $BuildDir/RPM/$Program.spec
#cat ../../src/changes.txt >> $BuildDir/RPM/$Program.spec
echo "creating installation..."
cp debian/changelog.Debian $BuildDir
cp ../../src/changes.txt $BuildDir/changelog

./copy_to_builddir.sh $Arch $BuildDir

cp ../../resources/world_icon64.png $BuildDir/icon_64.png
cp general/$Program.desktop $BuildDir
cp general/$Program.starter $BuildDir

mkdir -p $BuildDir/help
cp ../../help/* $BuildDir/help
mkdir -p $BuildDir/languages
cp ../../languages/*.po $BuildDir/languages
cp ../../languages/*.txt $BuildDir/languages
mkdir -p $BuildDir/importdata
cp -r ../../importdata/* $BuildDir/importdata

echo "building rpm ..."
sudo rpmbuild -ba --quiet $BuildDir/RPM/$Program.spec

sudo mv $(rpm --eval "%{_topdir}")/RPMS/$Arch/$Program*.rpm output/${Program}_${Version}_${Arch}-$Widgetset.rpm
sudo rm $(rpm --eval "%{_topdir}")/SRPMS/$Program*

echo "cleaning up..."
rm -r $BuildDir
