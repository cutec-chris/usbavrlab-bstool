#!/bin/bash
cp ../../output/$1-linux/prometheus $2
strip --strip-all $2/prometheus
cp ../../output/$1-linux/pstarter $2
strip --strip-all $2/pstarter
cp ../../output/$1-linux/updatedatabase $2
strip --strip-all $2/updatedatabase
cp ../../output/$1-linux/dbaserepair $2
strip --strip-all $2/dbaserepair
cp ../../output/$1-linux/dbsync $2
strip --strip-all $2/dbsync
Version=$(cat ../../src/version.inc).$(cat ../../src/revision.inc)
cp $2/prometheus ../executables/$Version
cp $2/pstarter ../executables/$Version
cp $2/updatedatabase ../executables/$Version
cp $2/dbaserepair ../executables/$Version
cp $2/dbsync ../executables/$Version
cp ../../output/$1-linux/cdmenue ../executables/$Version
strip --strip-all ../executables/$Version/cdmenue

