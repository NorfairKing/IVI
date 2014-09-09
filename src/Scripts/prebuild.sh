#!/bin/bash

scriptDirs=$(ls -d */)
for i in $scriptDirs
do 
    scriptDir=${i%%/} 
    if [ "$scriptDir" == "IVIBootstrap" ]
    then
        continue
    fi
    if [ -f $scriptDir/makefile ] || [ -f $scriptDir/Makefile ]
    then
        echo -n "prebuilding $scriptDir ... "
        cd $scriptDir
        make --quiet --no-print-directory
        cd ..
        echo "done"
    fi
done
