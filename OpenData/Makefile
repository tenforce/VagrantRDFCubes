all: rdf_cubes
	bash ./load_rdfcubes.sh

rdf_cubes:  rdf_cubes.pl
	prolog -f rdf_cubes.pl -t "all."

realclean:
	rm -rf *.rdf cyprus/*/*.rdf bulgaria/*/*.rdf
