#!/bin/bash
#
# Load all the created rdf data cubes files into virtuoso
#

set -x

CURL=curl
DBUSER=dba
DBPASSWD=root

# Add the graph to the application list of registered graphs
register_filegraph() { # $1 - filename
    curl http://localhost:8080/lod2webapi/register_graph -d "graph=$1"    
}

# For each of the RDF Datacubes load the file into a names graph
# and register the graph name.

for i in cyprus bulgaria 
do
    for f in $i/*
    do
	echo $i - $f;
	graph=`echo "file://$f.rdf" | tr -d [:space:] | tr [:upper:] [:lower:]`;
	echo -e "ld_dir('/vagrant_opendata\/$f', '*.rdf','$graph');\nrdf_loader_run();\nexec('checkpoint');" | isql-vt -U ${DBUSER} -P ${DBPASSWD} ;
	register_filegraph $graph
    done
done
echo -e "ld_dir('/vagrant_opendata', '*.rdf','http://opendata.tenforce.com');\nrdf_loader_run();\nexec('checkpoint');" | isql-vt -U ${DBUSER} -P ${DBPASSWD}
