SET PROGNAME=USB AVR-Lab Boundary Scan Tool
echo j|del ..\..\output\i386-win32\*.*
mkdir ..\..\output
mkdir ..\..\output\i386-win32
lazbuild ..\..\source\usbavrboundary.lpi
copy ..\..\output\i386-win32\usbavrboundary.exe 
strip --strip-debug usbavrboundary.exe
strip --strip-all usbavrboundary.exe

FOR /F %%L IN ('fpc -iTO') DO SET TARGETOS=%%L
FOR /F %%L IN ('fpc -iTP') DO SET TARGETCPU=%%L
FOR /F "delims='" %%F IN (..\..\source\version.inc) DO set VERSION=%%F
FOR /F "delims='" %%F IN (..\..\source\revision.inc) DO set VERSION=%VERSION%.%%F
SET FULLTARGET=%TARGETCPU%-%TARGETOS%-%VERSION%
iscc avrisptool.iss
del usbavrboundary.exe