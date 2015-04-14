#!/bin/bash
#
# Load all the created rdf data cubes files into virtuoso
#

set -x

DBUSER=dba
DBPASSWD=root

for i in cyprus bulgaria 
do
    for f in $i/*
    do
	echo $i - $f 
	echo -e "ld_dir('/vagrant_opendata/$f', '*.rdf',NULL);\nrdf_loader_run();\nexec('checkpoint');" | isql-vt -U ${DBUSER} -P ${DBPASSWD}
    done
done
echo -e "ld_dir('/vagrant_opendata', '*.rdf','http://opendata.tenforce.com');\nrdf_loader_run();\nexec('checkpoint');" | isql-vt -U ${DBUSER} -P ${DBPASSWD}
