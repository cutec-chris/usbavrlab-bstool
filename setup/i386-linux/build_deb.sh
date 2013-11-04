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
echo "creating control file..."
mkdir -p $BuildDir/DEBIAN
cat debian/control | \
  sed -e "s/VERSION/$Version/g" \
      -e "s/ARCH/$Arch/g" \
  > $BuildDir/DEBIAN/control
echo "copyright and changelog files..."
mkdir -p $BuildDir/usr/share/doc/$Program
cp debian/changelog.Debian $BuildDir/usr/share/doc/$Program/
cp ../../src/changes.txt $BuildDir/usr/share/doc/$Program/changelog
echo "creating installation..."
mkdir -p $BuildDir/usr/share/pixmaps/
mkdir -p $BuildDir/usr/share/applications
mkdir -p $BuildDir/usr/bin/
mkdir -p $BuildDir/usr/share/$Program
mkdir -p $BuildDir/usr/share/$Program/help
mkdir -p $BuildDir/usr/share/$Program/languages
mkdir -p $BuildDir/usr/share/$Program/importdata
install -m 644 ../../resources/world_icon64.png $BuildDir/usr/share/pixmaps/$Program.png
install -m 644 general/$Program.desktop $BuildDir/usr/share/applications/$Program.desktop
./copy_to_builddir.sh $Arch $BuildDir/usr/share/$Program
strip --strip-all $BuildDir/usr/share/$Program/$Program
cp general/$Program.starter $BuildDir/usr/share/$Program/
ln -s /usr/share/$Program/$Program.starter $BuildDir/usr/bin/$Program
cp ../../help/* $BuildDir/usr/share/$Program/help
cp -r ../../importdata/* $BuildDir/usr/share/$Program/importdata
cp ../../languages/*.po $BuildDir/usr/share/$Program/languages
cp ../../languages/*.txt $BuildDir/usr/share/$Program/languages
echo "building package..."
dpkg-deb --build $BuildDir
mv $TmpDir/software_build.deb output/${Program}_${Version}_${Arch}-$Widgetset.deb
echo "cleaning up..."
rm -r $BuildDir

