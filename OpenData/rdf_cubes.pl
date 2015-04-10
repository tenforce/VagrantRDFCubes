%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author  : P.A. Massey
% Date    : 2015
% Purpose : Quick script to convert some CSV files into the RDF Data Cube
%           and DCat formats.
% Note    : The main controlling structure for this is the description/X
%           clause. This has an ID which the convert/1 predicate uses to
%           recover this structure.
% Bugs    : 1) The resulting file has to be edited to add an xmlns="" 
%           attribute otherwise it won't be parsable.
%           2) XX

:- use_module(library(csv)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- rdf_load(library(semweb/rdfs)).

?- rdf_register_prefix(foaf,    'http://xmlns.com/foaf/0.1/').
?- rdf_register_prefix(qb,      'http://purl.org/linked-data/cube#').
?- rdf_register_prefix(rdf,     'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
?- rdf_register_prefix(rdfs,    'http://www.w3.org/2000/01/rdf-schema#').
?- rdf_register_prefix(xsd,     'http://www.w3.org/2001/XMLSchema#').
?- rdf_register_prefix(dcat,    'http://www.w3.org/ns/dcat#').
?- rdf_register_prefix(dcterms, 'http://purl.org/dc/terms/').
?- rdf_register_prefix(adms,    'http://www.w3.org/ns/adms#').
?- rdf_register_prefix(vcard,   'http://www.w3.org/2006/vcard/ns#').
?- rdf_register_prefix(dct,     'http://purl.org/dc/terms/').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main predicate. 

convert(Id):-
    description(Id,File,OutFile,Details,Dcat),        % Get the main description
		convert(Id,File,OutFile,Details,Dcat).

convert(Id,File,OutFile,Details,Dcat):-
    format(atom(DCatGraph),'DCat~a',[Id]),
		rdf_unload_graph(Id),                           
		rdf_unload_graph(DCatGraph),
		format(atom(BaseName),'http://mydata/~a#',[Id]),
		rdf_register_prefix(dsns,BaseName,[force(true)]),% WARNING: overwrites namespace
    load(description(Id,File,OutFile,Details)),      % Load the CSV file, etc.
    dump_main_headers(Id),
		create_triples(Id,File,Details),                 % Create the main triples
    format(atom(DCatGraph),'DCat~a',[Id]),
		dcat_assert(Id,DCatGraph,Dcat),                  % Create the DCAT triples
    !,
		format(atom(OutFileDs),'dcat_~a.rdf',[Id]),      % File for DCAT metadata
		rdf_save(OutFileDs,[base_uri(BaseName),graph(DCatGraph)]),
    rdf_save(OutFile,  [base_uri(BaseName),graph(Id)]).

dump_main_headers(Id):-
		format(atom(DSDName),'dsd_~a',[Id]),
		rdf_cube_ref(Id,EId),
		rdf_cube_ref(DSDName,CDSDName),
		rdf_current_ns(dsns,Ref),
		rdf_assert(Ref,rdf:type,owl:'Ontology',Id),
		rdf_assert(EId,rdf:type,qb:'DataSet',Id),
		rdf_assert(EId,qb:structure,CDSDName,Id),
		rdf_assert(EId,rdfs:label,Id),
		format(atom(Measure),'m~a',[Id]),
		rdf_cube_ref(Measure,CMeasure),
		rdf_assert(CMeasure,rdf:type,qb:'MeasureProperty',Id),
		rdf_assert(CMeasure,rdfs:label,literal(Measure),Id),
		format(atom(MeasureCS),'~a_CS',[Measure]),
		rdf_cube_ref(MeasureCS,CMeasureCS),
		rdf_assert(CMeasureCS,rdf:type,qb:'ComponentSpecification',Id),
		rdf_assert(CMeasureCS,rdfs:label,literal(MeasureCS),Id),
		rdf_assert(CMeasureCS,qb:measure,CMeasure,Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert all regardless of failures.

all :-
		convert(X),
		fail.
all.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Used throughout the rest to expand the reference with the given namespace.
% Namespace will be set once for each conversion.

rdf_cube_ref(In,Out):-
		rdf_current_ns(dsns,Ref),
		format(atom(Out),'~a~a',[Ref,In]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Load the CSV file into the internal database

load(description(Id,File,_,Description)) :-
    retractall(crow(_,_,_)),                            % Clean up from previous runs
  	csv_read_file(File, Rows, [functor(row)]),
  	label_rows(Rows,Id,1,NRows),
   	maplist(assert, NRows).

% Number the rows (but associate the description id with it)

label_rows([],_,_,_).
label_rows([R|T],RefId,RowNumber,[crow(RowNumber,R,RefId)|NT]):-
	NRowNumber is RowNumber + 1,
	label_rows(T,RefId,NRowNumber,NT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_triples(Id,File,Details) :-
		observations_to_triples(Id),        % Output the Observations
		dimensions_to_triples(Id,Details),  % Output the Dimension Details
		additional_descriptions(Id,File,Details),
		reduce_dimensions(Id,Details).

observations_to_triples(Id) :-
		observation(Id,Observation),
		observation_to_rdf_triples(Id,Observation),
		fail.
observations_to_triples(_).

observation_to_rdf_triples(Id,observation(Cell,Value,ObsDims)):-
		Value \= 'na',
		cell_ref(Cell,Ref),
    rdf_cube_ref(Ref,CRef),
    rdf_cube_ref(Id,CId),
		rdf_assert(CRef,qb:'dataSet',CId,Id),
		rdf_assert(CRef,rdf:type,qb:'Observation',Id),
		format(atom(Measure),'m~a',[Id]),
    rdf_cube_ref(Measure,CMeasure),
		rdf_assert(CRef,CMeasure,literal(type(xsd:double,Value)),Id),
		obsdimensions_to_rdf(Ref,ObsDims,Triples),
		rdf_assert_list(Id,Triples).
		
obsdimensions_to_rdf(_,[],[]).
obsdimensions_to_rdf(ObsRef,[DimCell-dimension(_,Name,_)|Tail],[Triple|NTail]):-
		cell_ref(DimCell,DimRef),
    rdf_cube_ref(Name,CName),
    rdf_cube_ref(ObsRef,CObsRef),
    rdf_cube_ref(DimRef,CDimRef),
		Triple = rdf(CObsRef,CName,CDimRef),
		obsdimensions_to_rdf(ObsRef,Tail,NTail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
dimensions_to_triples(Id,details(Dimensions,Range)):-
		dimlist_to_triples(Id,Dimensions).

dimlist_to_triples(_,[]).
dimlist_to_triples(Id,[dimension(Cell,Name,_)|Tail]):-
    rdf_cube_ref(Name,CName),
		rdf_assert(CName,rdf:type,qb:'DimensionProperty',Id),
		rdf_assert(CName,rdfs:label,literal(Name),Id),
		rdf_assert(CName,rdfs:range,CName,Id),
		format(atom(Measure),'m~a_CS',[Id]),
		format(atom(DSDName),'dsd_~a',[Id]),
		format(atom(NameCS),'~a_CS',[Name]),
    rdf_cube_ref(Measure,CMeasure),
    rdf_cube_ref(DSDName,CDSDName),
    rdf_cube_ref(NameCS,CNameCS),
		rdf_assert(CDSDName,rdf:type,qb:'DataStructureDefinition',Id),
		rdf_assert(CDSDName,qb:'component',CNameCS,Id),
		rdf_assert(CDSDName,qb:'component',CMeasure,Id),		
		rdf_assert(CNameCS,rdf:type,qb:'ComponentSpecification',Id),
		rdf_assert(CNameCS,rdfs:label,literal(NameCS),Id),
		rdf_assert(CNameCS,qb:'dimension',CName,Id),
		dimlist_to_triples(Id,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

additional_descriptions(Id,File,details(Dimensions,Range)):-
		add_dimension_descriptions(Id,Dimensions).

add_dimension_descriptions(_,[]).
add_dimension_descriptions(Id,[dimension(cols(CFrom,CTo),Name,[rows(RFrom,RTo)|_])|Rest]):-
		cells_in_range(cell(CFrom,RFrom),cell(CTo,RTo),[],L),
		add_dimension_cells(Id,L,Name),
		add_dimension_descriptions(Id,Rest).
add_dimension_descriptions(Id,[dimension(rows(RFrom,RTo),Name,[cols(CFrom,CTo)|_])|Rest]):-
		cells_in_range(cell(CFrom,RFrom),cell(CTo,RTo),[],L),
		add_dimension_cells(Id,L,Name),
		add_dimension_descriptions(Id,Rest).

add_dimension_cells(_,[],_).
add_dimension_cells(Id,[Cell|Rest],Name):-
		cell_ref(Cell,Ref),
		cell_value(Cell,Value),
    rdf_cube_ref(Ref,CRef),
    rdf_cube_ref(Name,CName),
		rdf_assert(CRef,rdf:type,CName,Id),
		rdf_assert(CRef,rdfs:label,literal(Value),Id),
		add_dimension_cells(Id,Rest,Name).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_dimensions(Id,details(DimensionList,_)):-
    reduce_dimensions_list(Id,DimensionList).

reduce_dimensions_list(_,[]).
reduce_dimensions_list(Id,[Dimension|Rest]):-
    ( to_reduce(Dimension) ->
         reduce_dimension(Id,Dimension)
    ;    true ),
    reduce_dimensions_list(Id,Rest).

to_reduce(dimension(_,Name,Operators)) :-
    member(reduce,Operators).

reduce_dimension(Id,dimension(_,Name,_)) :-
    rdf_cube_ref(Name,CName),
    setof(Label, S^( rdf(S,rdf:type,CName,Id),
                     rdf(S,rdfs:label,Label,Id) 
                   ), Labels),
    reduce_on_label(Id,Labels,Name).

reduce_on_label(_,[],_).
reduce_on_label(Id,[Label|Rest],Name):-
    rdf_cube_ref(Name,CName),
    setof(S, ( rdf(S,rdf:type,CName,Id),
               rdf(S,rdfs:label,Label,Id) ), [First|Rest0]),
    relabel(Id,Rest0,First),
    reduce_on_label(Id,Rest,Name).
    
relabel(_,[],_).
relabel(Id,[ToRelabel|Rest],First):-
    ( rdf(S,P,ToRelabel,Id) ->
        rdf_retractall(ToRelabel,_,_,Id),  % Delete original
        update_all(Id,ToRelabel,First)     % Update all object references
   ;   true ),
   relabel(Id,Rest,First).

update_all(Id,ToRelabel,New):-
   rdf(S,P,ToRelabel,Id),
   rdf_retractall(S,P,ToRelabel,Id),
   rdf_assert(S,P,New,Id),
   fail.
update_all(_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rdf_assert_list(_,[]).
rdf_assert_list(Id,[rdf(S,P,O)|Tail]):-
		rdf_assert(S,P,O,Id),
		rdf_assert_list(Id,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cells are described Col-Row, since that is a natural reading in 
% Excel (left-to-right, top-to-bottom).

observation(Id,observation(NCell,Value,ObservationDimensions)):-
    description(Id,_,_,details(Dimensions,data_range(cell(X,Y),cell(EX,EY))),_),
		cells_in_range(cell(X,Y),cell(EX,EY),[],NCells),
		member(NCell,NCells),       % On backtracking process successive cell observations
		cell_value(NCell,Value),
		cell_dimensions(NCell,Dimensions,ObservationDimensions).
					
% cell_in_range(FromCell,ToCell) Iterate over them

cells_in_range(cell(FromX,FromY),cell(ToX,ToY),SoFar,NCells):-
    ( FromY > ToY ->
					NCells = SoFar
    ;     cells_in_range(FromX,ToX,FromY,SoFar,NSoFar),
					NextFromY is FromY + 1,
					cells_in_range(cell(FromX,NextFromY),cell(ToX,ToY),NSoFar,NCells)).
    
cells_in_range(FromX,ToX,FromY,CellsIn,CellsOut) :-
    ( FromX =< ToX ->
					NCellsIn = [cell(FromX,FromY)|CellsIn],
					NFromX is FromX + 1,
					cells_in_range(NFromX,ToX,FromY,NCellsIn,CellsOut)
    ;     CellsOut = CellsIn ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Recover the cell Dimension descriptions

cell_dimensions(Cell,Dimensions,DimensionsInvolved):-
    cell_dimensions(cell_in_dimension_range,Cell,Dimensions,DimensionsInvolved).

cell_dimensions(_,_,[],[]).
cell_dimensions(CellTest, Cell,[Dimension|Tail],RelevantDimensions):-
    ( call(CellTest,Cell,Dimension,ICell) ->
					RelevantDimensions = [ICell-Dimension|NewTail],
					cell_dimensions(CellTest,Cell,Tail,NewTail)
    ;     cell_dimensions(CellTest,Cell,Tail,RelevantDimensions)).


% Test if the cell in the dimension description.
		
cell_in_dimension(cell(X,Y),dimension(cols(From,To),Desc,[Rows|_]),NCell):-
    ( X >= From, X =< To ->
					( Rows = rows(RFrom,RTo), Y >= RFrom, Y =< RTo ->
								NCell = cell(X,Y)
						; fail )  
			;    fail ).
cell_in_dimension(cell(X,Y),dimension(rows(From,To),Desc,[Cols|_]),NCell):-
    ( Y >= From, Y =< To ->
					( Cols = cols(CFrom,CTo), X >= CFrom, X=< CTo ->
								NCell = cell(X,Y)
						; fail )
 			;    fail ).

% Test if the cell is in the dimension.
		
cell_in_dimension_range(cell(X,Y),dimension(cols(From,To),Desc,[Rows|_]),NCell):-
    ( Rows = rows(RFrom,RTo), Y >= RFrom, Y =< RTo ->
					NCell = cell(From,Y)
			;   fail ).
cell_in_dimension_range(cell(X,Y),dimension(rows(From,To),Desc,[Cols|_]),NCell):-
    ( Cols = cols(CFrom,CTo), X >= CFrom, X=< CTo ->
					NCell = cell(X,From)
			; fail ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Recover the cell value

cell_value(cell(C,R),Value):-
    get_row(R,Row),
    arg(C,Row,Value).

cell_ref(cell(C,R),Ref):-
		format(atom(Ref),'c~a-r~a',[C,R]).

% Recover the row value

get_row(X,Row):-
    crow(X,Row,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_cell_description(Id,cell(Col,Row,Descriptions)):-
    cell_ref(cell(Col,Row),Ref),
    rdf_cube_ref(Ref,ObsRef),
    add_cell_description_list(Id,ObsRef,Descriptions).

add_cell_description_list(_,_,[]).
add_cell_description_list(Id,ObsRef,[C|Tail]):-
    ( C = comment:Comment ->
        rdf_assert(ObsRef,rdfs:comment,Comment,Id)
    ;   true ),
    add_cell_description_list(Id,ObsRef,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(ID) :-
    description(ID,File,Out,Description,_),
    load(description(ID,File,Out,Description)).

test(ID,Cell,DimensionsInvolved):-
    description(ID,File,Out,details(Dimensions,Range),_),
    cell_dimensions(cell_in_dimension,Cell,Dimensions,DimensionsInvolved).

test_in_range(ID,Cell,DimensionsInvolved):-
    description(ID,File,Out,details(Dimensions,Range),_),
    cell_dimensions(cell_in_dimension_range,Cell,Dimensions,DimensionsInvolved).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dcat(FileId,Authority,AccessUrl,CountryName,Title,Description,Issued,Name,
     HomePage,
     dcat(FileId,Authority,AccessUrl,CountryName,Title,Description,
          Issued,Name,HomePage)).

dcat_cyprus(FileId,Authority,AccessUrl,CountryName,
            Title,Description,Issued,Name,Dcat):-
     HomePage = 'http://www.cystat.gov.cy/open-data',
     dcat(FileId,Authority,AccessUrl,CountryName,Title,Description,Issued,
          Name,HomePage,Dcat).

dcat_bulgaria(FileId,Authority,AccessUrl,CountryName,
            Title,Description,Issued,Name,Dcat):-
     HomePage = 'http://bulgaria/homepage',
     dcat(FileId,Authority,AccessUrl,CountryName,Title,Description,Issued,Name,
          HomePage,Dcat).

dcat_assert(Id,DCatGraph,DcatIn) :-
  DcatIn = dcat(FileId,Authority,AccessUrl,
                CountryName,Title,Description,Issued,Name,HomePage),
  format(atom(CatId),'cat_~a',[CountryName]),
  format(atom(CatTitle),'Open Data Support high Value Dataset ~a',[CountryName]),
  format(atom(CatDescription),'High Value Dataset Catalogue produced by the EC project Open Data Support for the Statistical office of ~a',[CountryName]),
  format(atom(PubId),'pub_~a',[CountryName]),
  format(atom(DsId),'ds_~a',[Id]),
  format(atom(OrgId),'org_~a',[Id]),
  format(atom(OrgIdAdd),'orgadd_~a',[Id]),
  format(atom(DisId),'disxls_~a',[Id]),
  format(atom(DisRDFId),'disrdf_~a',[Id]),
  format(atom(DsRecId),'dsrec_~a',[Id]),
  format(atom(DisRDFFileId),'~a/files/~a.rdf',[HomePage,Id]),

  rdf_cube_ref(Id,CId),
  rdf_cube_ref(PubId,CPubId),
  rdf_assert(CId,dct:title,literal(Title),Id),
  rdf_assert(CId,dct:description,literal(Description),Id),
  rdf_assert(CId,dct:publisher,CPubId,Id),
  rdf_assert(CId,dct:issued,literal(Issued),Id),
  rdf_assert(CId,dct:identifier,literal(FileId),Id),

	rdf_assert(CPubId,rdf:type,foaf:'Agent',Id),
	rdf_assert(CPubId,foaf:name,literal(Name),Id),
	rdf_assert(CPubId,dcterms:type,
             'http://purl.org/adms/publishertype/NationalAuthority',Id),

  % From here on, the triples are placed in the Dcat Graph

	rdf_assert(PubId,rdf:type,foaf:'Agent',Id),
	rdf_assert(PubId,foaf:name,literal(Name),Id),
	rdf_assert(PubId,dcterms:type,
             'http://purl.org/adms/publishertype/NationalAuthority',Id),

  rdf_assert(CatId,rdf:type,dcat:'Catalog',DCatGraph),
	rdf_assert(CatId,dcterms:title,literal(CatTitle),DCatGraph),
	rdf_assert(CatId,dcterms:description,literal(CatDescription),DCatGraph),
	rdf_assert(CatId,dcterms:publisher,PubId,DCatGraph),
	rdf_assert(CatId,dcat:dataset,DsId,DCatGraph),
	rdf_assert(CatId,dcterms:issued,literal(Issued),DCatGraph),
  rdf_assert(CatId,foaf:homepage,HomePage,DCatGraph),
  rdf_assert(CatId,dcterms:language,'http://id.loc.gov/vocabulary/iso639-1/en',DCatGraph),
  rdf_assert(CatId,dcat:record, DsRecId,DCatGraph),

	rdf_assert(PubId,rdf:type,foaf:'Agent',DCatGraph),
	rdf_assert(PubId,foaf:name,literal(Name),DCatGraph),
	rdf_assert(PubId,dcterms:type,'http://purl.org/adms/publishertype/NationalAuthority',DCatGraph),

  rdf_assert(DsRecId,rdf:type,dcat:'CatalogRecord',DCatGraph),
  rdf_assert(DsRecId,foaf:primaryTopic,DsId,DCatGraph),
  rdf_assert(DsRecId,dcterms:modified,literal(Issued),DCatGraph),
  rdf_assert(DsRecId,dcterms:issued,literal(Issued),DCatGraph),

	rdf_assert(DsId,rdf:type,dcat:'Dataset',DCatGraph),
	rdf_assert(DsId,dcterms:title,literal(Title),DCatGraph),
	rdf_assert(DsId,dcterms:description,literal(Description),DCatGraph),
	rdf_assert(DsId,adms:contactPoint,OrgId,DCatGraph),
	rdf_assert(DsId,dcat:distribution,DisId,DCatGraph),
	rdf_assert(DsId,dcat:distribution,DisRDFId,DCatGraph),
	rdf_assert(DsId,dcterms:publisher,PubId,DCatGraph),
	rdf_assert(DsId,dcterms:issued,literal(Issued),DCatGraph),
	rdf_assert(DsId,dcterms:theme,literal('Statistics'),DCatGraph),
  rdf_assert(DsId,dcterms:language,'http://id.loc.gov/vocabulary/iso639-1/en',DCatGraph),
  
	rdf_assert(OrgId,rdf:type,vcard:'VCard',DCatGraph),
	rdf_assert(OrgId,rdf:type,vcard:'Organisation',DCatGraph),
  rdf_assert(OrgId,vcard:hasAddress,OrgIdAdd,DCatGraph),
	rdf_assert(OrgIdAdd,rdf:type,vcard:'Address',DCatGraph),
	rdf_assert(OrgIdAdd,vcard:'country-name',literal(CountryName),DCatGraph),

  % Basic Distribution
	rdf_assert(DisId,rdf:type,dcat:'Distribution',DCatGraph),
  rdf_assert(DisId,dcterms:description,literal('A tabular format of the data'),DCatGraph),
  rdf_assert(DisId,dcterms:'format','http://publications.europa.eu/resource/authority/file-type/XLSX',DCatGraph),
  rdf_assert(DisId,dcterms:mediaType,literal('application/vnd.ms-excel'),DCatGraph),
  rdf_assert(DisId,dcterms:'license',literal('License'),DCatGraph),
	rdf_assert(DisId,dcterms:'release_date',literal(Issued),DCatGraph),
  rdf_assert(DisId,dcat:accessURL,AccessUrl,DCatGraph),
  rdf_assert(DisId,dcat:downloadURL,AccessUrl,DCatGraph),
  rdf_assert(DisId,dcat:status,
             'http://purl.org/adms/status/UnderDevelopment',DCatGraph),

  % The RDF dictribution of the files
	rdf_assert(DisRDFId,rdf:type,dcat:'Distribution',DCatGraph),
  rdf_assert(DisRDFId,dcterms:description,literal('The data in RDF format according to the DataCube vocabulary'),DCatGraph),
  rdf_assert(DisRDFId,dcterms:'format','http://publications.europa.eu/resource/authority/file-type/RDF_XML',DCatGraph),
  rdf_assert(DisRDFId,dcterms:mediaType,literal('application/rdf+xml'),DCatGraph),
  rdf_assert(DisRDFId,dcterms:'license',literal('License'),DCatGraph),
	rdf_assert(DisRDFId,dcterms:'release_date',literal(Issued),DCatGraph),
  rdf_assert(DisRDFId,dcat:accessURL,DisRDFFileId,DCatGraph),
  rdf_assert(DisRDFId,dcat:downloadURL,DisRDFFileId,DCatGraph),
  rdf_assert(DisRDFId,dcat:status,
             'http://purl.org/adms/status/UnderDevelopment',DCatGraph).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creates references for the work directory structure.

workfile_cyprus_references(FileId,Sheet,File,FileOut) :-
    workfile_references('cyprus',FileId,Sheet,File,FileOut).
workfile_bulgaria_references(FileId,Sheet,File,FileOut) :-
    workfile_references('bulgaria',FileId,Sheet,File,FileOut).

    % Generic version

workfile_references(BaseDir,FileId,Sheet,File,FileOut) :-
    format(atom(MainDir),'~a/~a',[BaseDir,FileId]),
    ( Sheet = 'no' ->
         format(atom(Root),'~a/~a',[MainDir,FileId])
    ;    format(atom(Root),'~a/~a-~a',[MainDir,FileId,Sheet]) ),
    format(atom(File),'~a.csv',[Root]),
    format(atom(FileOut),'~a-cube.rdf',[Root]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Descriptions of the transformations are given below

description(edInd_a1,File,OutFile,Description,Dcat):-
		FileId = 'EDUCATION_INDICATORS-A85_86-11_12-EN-210613',
    workfile_cyprus_references(FileId,'A1',File,OutFile),
    Title = 'HOUSEHOLDS, INSTITUTIONS AND POPULATION ENUMERATED BY SEX AND DISTRICT (1.10.2011)',
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                Title,Title,'23/05/2012','Name',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimDistrict',
																		 [rows(3,7)]),
													 dimension(rows(1,1),
																		 'PopulationGrouping',
																		 [cols(2,10),reduce]),
													 dimension(rows(2,2),
																		 'PopulationType',
																		 [cols(2,10),reduce])],
													data_range(cell(2,3),cell(10,7))).
description(edInd_a2,File,OutFile,Description,Dcat):-
		FileId = 'EDUCATION_INDICATORS-A85_86-11_12-EN-210613',
    workfile_cyprus_references(FileId,'A2',File,OutFile),
    Title = 'LIVING QUARTERS, HOUSEHOLDS, INSTITUTIONS AND POPULATION, ENUMERATED BY DISTRICT  (1.10.2011)',
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                Title,Title,'23/05/2012','Name',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimDistrict',
																		 [rows(4,9)]),
													 dimension(rows(2,2),
																		 'DimGroupingType',
																		 [cols(2,9),reduce]),
													 dimension(rows(2,2),
																		 'DimPopulation',
																		 [cols(2,9),reduce])],
													data_range(cell(2,4),cell(9,9))).
description(edInd_b1,File,OutFile,Description,Dcat):-
		FileId = 'EDUCATION_INDICATORS-A85_86-11_12-EN-210613',
    workfile_cyprus_references(FileId,'B1',File,OutFile),
    Title = 'OCCUPIED CONVENTIONAL DWELLINGS ENUMERATED, BY YEAR OF CONSTRUCTION (COMPLETION) OF THE DWELLING, TYPE OF BUILDING IN WHICH THE DWELLING IS LOCATED, TENURE STATUS AND URBAN/RURAL AREA (1.10.2011)',
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                Title,Title,'09/01/2014','Name',Dcat),
    Description = details([dimension(cols(1,1),
                                     'DimLivingType',
                                     [rows(6,291),reduce]),
                           dimension(cols(2,2),
																		 'DimTenureStatus',
																		 [rows(6,291),reduce]),
													 dimension(cols(3,3),
																		 'DimConstructionYear',
																		 [rows(6,291),reduce]),
													 dimension(rows(2,2),
																		 'DimBuildingGroup',
																		 [cols(4,13),reduce]),
													 dimension(rows(3,3),
																		 'DimBuildingType',
																		 [cols(4,13),reduce])],
													data_range(cell(4,6),cell(13,291))).
description(edInd_b3,File,OutFile,Description,Dcat):-
		FileId = 'EDUCATION_INDICATORS-A85_86-11_12-EN-210613',
    workfile_cyprus_references(FileId,'B3',File,OutFile),
    Title = 'HOUSEHOLDS BY TYPE OF LIVING QUARTER/TENURE STATUS (FOR HOUSEHOLDS IN CONVENTIONAL DWELLINGS)/DISTRICT AND URBAN/RURAL AREA (1.10.2011)',
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                Title,Title,'09/01/2014','Name',Dcat),
    Description = details([dimension(cols(1,1),
                                     'DimType',
                                     [rows(4,20),reduce]),
                           dimension(cols(2,2),
																		 'DimDistrict',
																		 [rows(4,20),reduce]),
													 dimension(rows(2,2),
																		 'DimLivingType',
																		 [cols(3,12),reduce]),
													 dimension(rows(3,3),
																		 'DimTenureStatus',
																		 [cols(3,12),reduce])],
													data_range(cell(3,4),cell(12,20))).
description(energy,File,OutFile,Description,Dcat):-
		FileId = 'ENERGY_STATISTICS-A2013-131114',
    workfile_cyprus_references(FileId,'S1',File,OutFile),
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                'ENERGY_STATISTICS-A2013-131114',
                'Description','Issued','Name',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimYear',
																		 [rows(3,56)]),
													 dimension(rows(1,1),
																		 'DimOilUsed',
																		 [cols(2,14)])],
													data_range(cell(3,2),cell(14,56))).
description(eusocialgdp,File,OutFile,Description,Dcat):-
    % Social Expenditure as percentage of GDP
		FileId = 'EUSPROT-ANNUAL_2000-2011-EN-021213',
    workfile_cyprus_references(FileId,'no',File,OutFile),
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                'EUSPROT-ANNUAL_2000-2011-EN-021213',
                'Description','Issued','Name',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimRegion',
																		 [rows(5,32)]),
													 dimension(rows(3,3),
																		 'DimYear',
																		 [cols(2,13)]),
													 dimension(rows(4,4),
																		 'DimRegionGroup',
																		 [cols(2,13),reduce])],
													data_range(cell(2,5),cell(13,32))).
description(airemissions,File,OutFile,Description,Dcat):-
    % Social Expenditure as percentage of GDP
		FileId = 'EMISSIONS_OF_AIR_POLLUTANTS_A1990_12-EN-120914',
    workfile_cyprus_references(FileId,'headings',File,OutFile),
    dcat_cyprus('FileId','Authority','AccessUrl','Cyprus',
                'EMISSIONS_OF_AIR_POLLUTANTS_A1990_12-EN-120914',
                'Description','Issued','Name',Dcat),
    Description = details([dimension(rows(2,2),
																		 'DimYear',
																		 [cols(2,20)]),
													 dimension(cols(1,1),
																		 'DimSource',
																		 [rows(4,12)])],
													data_range(cell(2,4),cell(20,12))).
description(gdpgrowth2013Q2,File,OutFile,Description,Dcat):-
    FileId = 'Growth_Rates_CP-Q314-EN-091214',
    workfile_cyprus_references(FileId,'no',File,OutFile),
    Title = 'GROWTH RATES OF GDP AT CONSTANT PRICES, 3rd QUARTER 2014',
    AccessUrl = 'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/Growth_Rates_CP-Q314-EN-091214.xls?OpenElement',
    dcat_cyprus(FileId,'Cyprus Stats Office',AccessUrl,'Cyprus',
                'GROWTH RATES OF GDP AT CONSTANT PRICES, 3rd QUARTER 2014',
                'GROWTH RATES OF GDP AT CONSTANT PRICES, 3rd QUARTER 2014',
                '01/09/2014','Name',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimYear',
																		 [rows(3,22),reduce]),
													 dimension(cols(2,2),
																		 'DimQuarter',
																		 [rows(3,22),reduce]),
                           dimension(rows(2,2),
                                     'DimQuestion',
                                     [cols(3,5)])], 
													data_range(cell(3,3),cell(5,22))).
description(tourism2014,File,OutFile,Description,Dcat):-
    FileId = 'TOURISM_MONTHLY_ARRIVALS-2014-EN-190115',
    workfile_cyprus_references(FileId,'no',File,OutFile),
    Title = 'ARRIVALS  OF  TOURISTS  BY COUNTRY  OF USUAL  RESIDENCE  AND  MONTH (2014)',
    AccessUrl = 'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/TOURISM_MONTHLY_ARRIVALS-2014-EN-190115.xls?OpenElement',
    dcat_cyprus(FileId,'Cyprus Stats Office',AccessUrl,'Cyprus',
         'ARRIVALS  OF  TOURISTS  BY COUNTRY  OF USUAL  RESIDENCE  AND  MONTH (2014)',
         'ARRIVALS  OF  TOURISTS  BY COUNTRY  OF USUAL  RESIDENCE  AND  MONTH',
         '10/01/2015','Name',Dcat),
    Description = details([dimension(rows(2,2),
																		 'DimMonth',
																		 [cols(2,13)]),
													 dimension(cols(1,1),
																		 'DimResidence',
																		 [rows(4,78)])],
													data_range(cell(2,4),cell(13,78))).
description(harmonizedprinceindex2014,File,OutFile,Description,Dcat):-
    FileId = 'HICP-INDEX-1214-EN-130115',
    workfile_cyprus_references(FileId,'no',File,OutFile),
    Title = 'HARMONIZED INDEX OF CONSUMER PRICES, 2014',
    AccessUrl = 'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/HICP-INDEX-1214-EN-130115.xls?OpenElement',
    dcat_cyprus(FileId,'Cyprus Stats Office',AccessUrl,'Cyprus',
         'HARMONIZED INDEX OF CONSUMER PRICES, 2014',
         'HARMONIZED INDEX OF CONSUMER PRICES, 2014 (price indicies 2005=100)',
         '13/01/2015','Name',Dcat),
    Description = details([dimension(rows(4,4),
																		 'DimMonth',
																		 [cols(3,14)]),
													 dimension(cols(2,2),
																		 'DimIndexGroup',
																		 [rows(5,17)])],
													data_range(cell(3,5),cell(14,17))).
description(harmonizedprinceindex2013cmp2014,File,OutFile,Description,Dcat):-
    FileId = 'HICP-CHANGEMONTH-1214-EN-130115',
    workfile_cyprus_references(FileId,'no',File,OutFile),
    Title = 'HARMONIZED INDEX OF CONSUMER PRICES, 2014',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'Cyprus Stats Office',AccessUrl,'Cyprus',
         'HARMONIZED INDEX OF CONSUMER PRICES, 2014 (cmpared with 2013)',
         '% CHANGE COMPARED WITH CORRESPONDING MONTH OF 2013',
         '13/01/2015','Name',Dcat),
    Description = details([dimension(rows(4,4),
																		 'DimMonth',
																		 [cols(3,14)]),
													 dimension(cols(2,2),
																		 'DimIndexGroup',
																		 [rows(5,17)])],
													data_range(cell(3,5),cell(14,17))).
description(agrifarmstruct_s1,File,OutFile,Description,Dcat):-
    FileId = 'AGRI-FARM_STRUCT-A2013-EN-301214',
    workfile_cyprus_references(FileId,'S1',File,OutFile),
    Title = 'HOLDINGS AND UTILIZED AGRICULTURAL AREA BY TYPE AND HOLDERS DISTRICT OF RESIDENCE, 2013',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,'Cyprus',
         Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimDistrict',
																		 [rows(5,10)]),
                           dimension(rows(3,3),
                                     'DimAgriType',
                                     [cols(2,9),reduce]),
													 dimension(rows(4,4),
																		 'DimAgriSize',
																		 [cols(2,9),reduce])],
													data_range(cell(2,5),cell(9,10))).
description(agrifarmstruct_s2,File,OutFile,Description,Dcat):-
    FileId = 'AGRI-FARM_STRUCT-A2013-EN-301214',
    workfile_cyprus_references(FileId,'S2',File,OutFile),
    Title = 'HOLDINGS AND UTILIZED AGRICULTURAL AREA BY LEGAL STATUS OF HOLDER AND BY HOLDERS DISTRICT OF RESIDENCE, 2013',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'Cyprus',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimDistrict',
																		 [rows(7,12)]),
                           dimension(rows(4,4),
                                     'DimHoldingType',
                                     [cols(2,11),reduce]),
													 dimension(rows(5,5),
																		 'DimHoldingGroup',
																		 [cols(2,11),reduce]),
                           dimension(rows(6,6),
                                     'DimHoldingArea',
                                     [cols(2,11),reduce])],
													data_range(cell(2,7),cell(11,12))).
description(agrifarmstruct_s3,File,OutFile,Description,Dcat):-
    FileId = 'AGRI-FARM_STRUCT-A2013-EN-301214',
    workfile_cyprus_references(FileId,'S3',File,OutFile),
    Title = 'COMPOSITION OF THE TOTAL AGRICULTURAL AREA OF THE HOLDINGS BY HOLDERS DISTRICT OF RESIDENCE (2013)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimDistrict',
																		 [rows(5,10)]),
                           dimension(rows(2,2),
                                     'DimAreaType',
                                     [cols(2,10),reduce]),
													 dimension(rows(3,3),
																		 'DimGroup',
																		 [cols(2,10),reduce]),
                           dimension(rows(4,4),
                                     'DimHoldingArea',
                                     [cols(2,10),reduce])],
													data_range(cell(2,5),cell(10,10))).
description(agrifarmstruct_s4,File,OutFile,Description,Dcat):-
    FileId = 'AGRI-FARM_STRUCT-A2013-EN-301214',
    workfile_cyprus_references(FileId,'S4',File,OutFile),
    Title = 'AREA BY CROP TYPE/BY HOLDERS DISTRICT OF RESIDENCE (2013)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(2,2),
																		 'DimDistrict',
																		 [cols(2,7)]),
                           dimension(rows(3,3),
                                     'DimCropArea',
                                     [cols(2,7),reduce]),
													 dimension(cols(1,1),
																		 'DimCropType',
																		 [rows(4,28),reduce])],
													data_range(cell(2,4),cell(7,28))).
description(agrifarmstruct_s5,File,OutFile,Description,Dcat):-
    FileId = 'AGRI-FARM_STRUCT-A2013-EN-301214',
    workfile_cyprus_references(FileId,'S5',File,OutFile),
    Title = 'NUMBER OF ANIMALS BY TYPE AND HOLDERS DISTRICT OF RESIDENCE (2013)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(2,2),
																		 'DimDistrict',
																		 [cols(2,7)]),
                           dimension(cols(1,1),
                                     'DimAnimalType',
                                     [rows(3,17)])],
													data_range(cell(2,2),cell(7,17))).
description(agrifarmstruct_s7,File,OutFile,Description,Dcat):-
    FileId = 'AGRI-FARM_STRUCT-A2013-EN-301214',
    workfile_cyprus_references(FileId,'S7',File,OutFile),
    Title = 'EMPLOYMENT BY CATEGORY AND HOLDERS DISTRICT OF RESIDENCE (2013)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimDistrict',
																		 [rows(5,10)]),
                           dimension(rows(2,2),
                                     'DimEmploymentCategory',
                                     [cols(2,14),reduce]),
                           dimension(rows(3,3),
                                     'DimType',
                                     [cols(2,14),reduce]),
                           dimension(rows(4,4),
                                     'DimGender',
                                     [cols(2,14),reduce])],
													data_range(cell(2,5),cell(14,10))).
description(emissiongases,File,OutFile,Description,Dcat):-
    FileId = 'EMISSIONS_OF_GREENHOUSE_GASES_A1990_12-EN-180914',
    workfile_cyprus_references(FileId,'S0',File,OutFile),
    Title = 'EMISSIONS OF GREENHOUSE GASES (Quantities in thousand tonnes of CO2 equivalent)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimYear',
																		 [rows(4,26)]),
                           dimension(rows(3,3),
                                     'DimGas',
                                     [cols(2,6),reduce])],
													data_range(cell(2,4),cell(6,26))).
description(innovation2010to2011,File,OutFile,Description,Dcat):-
    FileId = 'INNOVATION_2008-10-EN-301013',
    workfile_cyprus_references(FileId,'S0',File,OutFile),
    Title = 'INNOVATION_2008-10-EN-301013',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'24/10/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimNACECode',
																		 [rows(4,36),reduce]),
                           dimension(cols(2,2),
                                     'DimEconomicActivity',
                                     [rows(4,36),reduce]),
                           dimension(cols(3,3),
                                     'DimClassSize',
                                     [rows(4,36),reduce]),
                           dimension(rows(3,3),
                                     'DimEnterprises',
                                     [cols(4,10),reduce])],
													data_range(cell(4,4),cell(10,36))).
description(industry_s1,File,OutFile,Description,Dcat):-
    FileId = 'INDUSTRY-A05_13-EN-241014',
    workfile_cyprus_references(FileId,'S1',File,OutFile),
    Title = 'INNOVATION_2008-10-EN-301013 (GROSS OUTPUT)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'24/10/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimNACECode',
																		 [rows(4,35),reduce]),
                           dimension(cols(2,2),
                                     'DimEconomicActivity',
                                     [rows(4,35),reduce]),
                           dimension(rows(3,3),
                                     'DimPersonsEngagedInYear',
                                     [cols(3,11),reduce])],
													data_range(cell(3,4),cell(11,35))).
description(industry_s2,File,OutFile,Description,Dcat):-
    FileId = 'INDUSTRY-A05_13-EN-241014',
    workfile_cyprus_references(FileId,'S2',File,OutFile),
    Title = 'INNOVATION_2008-10-EN-301013 (VALUE ADDED)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'24/10/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimNACECode',
																		 [rows(4,35),reduce]),
                           dimension(cols(2,2),
                                     'DimEconomicActivity',
                                     [rows(4,35),reduce]),
                           dimension(rows(3,3),
                                     'DimPersonsEngagedInYear',
                                     [cols(3,9),reduce])],
													data_range(cell(3,4),cell(9,35))).
description(industry_s3,File,OutFile,Description,Dcat):-
    FileId = 'INDUSTRY-A05_13-EN-241014',
    workfile_cyprus_references(FileId,'S3',File,OutFile),
    Title = 'INNOVATION_2008-10-EN-301013 (INDUSTRY)',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/10/2010','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimNACECode',
																		 [rows(4,35),reduce]),
                           dimension(cols(2,2),
                                     'DimEconomicActivity',
                                     [rows(4,35),reduce]),
                           dimension(rows(3,3),
                                     'DimPersonsEngagedInYear',
                                     [cols(3,11),reduce])],
													data_range(cell(3,4),cell(11,35))).
description(industryprod,File,OutFile,Description,Dcat):-
    FileId = 'IND_PRODUCTION-M-00_1014-EN-301214',
    workfile_cyprus_references(FileId,'no',File,OutFile),
    Title = 'INDEX OF INDUSTRIAL PRODUCTION',
    format(atom(AccessUrl),
           'http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/EB2B4ACD559F9047C2257D480027D4CC/$file/~a.xls?OpenElement',
           [FileId]),
    dcat_cyprus(FileId,'REPUBLIC OF CYPRUS, STATISTICAL SERVICE',AccessUrl,
         'REPUBLIC OF CYPRUS',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(cols(1,1),
																		 'DimYear',
																		 [rows(5,19)]),
                           dimension(rows(4,4),
                                     'DimMonths',
                                     [cols(2,14)])],
													data_range(cell(2,3),cell(14,19))).
description(en_jam,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_jam',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_jam',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_lov,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_lov',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_lov',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_szr,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_szr',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_szr',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_bg,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_bg',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_bg',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_blg,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_blg',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_blg',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_blg,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_bgs',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_bgs',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_per,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_per',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_per',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_pvn,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_pvn',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_pvn',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_sml,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_sml',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_sml',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_vid,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_vid',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_vid',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_vrc,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_vrc',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_vrc',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_vtr,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_vtr',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_vtr',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
         'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
description(en_mon,File,OutFile,Description,Dcat):-
    FileId = 'Regio Internet (2007-2012)_en_mon',
    workfile_bulgaria_references(FileId,'no',File,OutFile),
    Title = 'Regio Internet (2007-2012)_en_mon',
    format(atom(AccessUrl),
           'http://www.google.com',
           [FileId]),
    dcat_cyprus(FileId,'Bulgaria, STATISTICAL SERVICE',AccessUrl,
                'Bulgaria',Title,Title,'30/12/2014','STATISTICAL SERVICE',Dcat),
    Description = details([dimension(rows(3,3),
																		 'DimYear',
																		 [cols(3,8)]),
                           dimension(cols(2,2),
                                     'DimIndicators',
                                     [rows(4,45)])],
													data_range(cell(3,4),cell(8,45))).
