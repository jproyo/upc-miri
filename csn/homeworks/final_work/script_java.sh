#!/bin/bash
PACKAGE_AVOID=$1
shift;
JARS_PATH=$@
java -jar /Users/juan/.m2/repository/gr/gousiosg/javacg/0.1-SNAPSHOT/javacg-0.1-SNAPSHOT-static.jar $JARS_PATH | grep -v "java\..*$" | grep $PACKAGE_AVOID > result.txt
sed -e 's/[M|O|I|S|D]://g' -e 's/([M|O|I|S|D])//g' -e 's/()//g' -e 's/(.*) / /g' -e 's/(.*)$//g' -e 's/:<init>//g' -e 's/\./_/g' -e 's/\:/_/g' -e 's/ / -> /g' -i bkp result.txt 