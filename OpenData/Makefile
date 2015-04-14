all: rdf_cubes
	./load_rdfcubes.sh

rdf_cubes:  rdf_cubes.pl
	prolog -f rdf_cubes.pl -t "all."

realclean:
	rm -rf *.rdf cyprus/*/*.rdf bulgaria/*/*.rdf
