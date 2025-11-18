:- module(rdf_loader,
    [
        load_rdf_facts/1,
        course/3,           % course(ID, Name, Credits)
        degree/2            % degree(ID_Gelar, NamaGelar) e.g., degree('ST', 'S.T.')
    ]).

% Import predicate
:- use_module(library(semweb/rdf_db), [
    rdf_load/1,
    rdf/3,
    rdf_global_id/2,
    rdf_register_ns/2
]).

% Dynamic predicates
:- dynamic course/3.
:- dynamic degree/2.

:- rdf_db:rdf_register_ns(cc, 'http://www.semanticweb.org/marvinscifo/ontologies/2025/10/college-course#').
:- rdf_db:rdf_register_ns(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_db:rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

% ===========================================
% Load RDF Facts
% ===========================================

load_rdf_facts(File) :-
    % clear old facts
    retractall(course(_, _, _)),
    retractall(degree(_, _)),
    
    write('Loading RDF file...'), nl,
    rdf_load(File),
    write('RDF loaded into triple store.'), nl,

    % Ekstrak new facts
    extract_all_courses,
    extract_all_degrees,
    
    write('RDF facts extracted successfully!'), nl.

get_id(URI, ID) :-
    atom(URI),
    rdf_global_id(URI, URIAtom),
    split_string(URIAtom, "#", "", [_, IDStr]),
    atom_string(ID, IDStr).

extract_all_courses :-
    findall(
        (ID, Name, Credits),
        (   
            rdf(CourseURI, rdf:type, cc:'Courses'), 
            
            opt_rdf(CourseURI, cc:hasCourseID, literal(ID), 'UNKNOWN_ID'),
            opt_rdf(CourseURI, cc:hasCourseName, literal(Name), 'Unknown Name'),

            opt_rdf(CourseURI, cc:hasCourseCredit, literal(CreditStr), '0'),
            atom_number(CreditStr, Credits)

        ),
        Courses
    ),
    length(Courses, Count),
    assert_courses(Courses),
    format('Loaded ~w courses~n', [Count]).

assert_courses([]).
assert_courses([(ID, Name, Credits)|Rest]) :-
    assertz(course(ID, Name, Credits)),
    assert_courses(Rest).

extract_all_degrees :-
    findall(ID, (rdf(GelarURI, rdf:type, cc:'BacheloralDegree'), get_id(GelarURI, ID)), IDs1),
    forall(member(ID, IDs1), assertz(degree(ID, 'S.T.'))),
    
    findall(ID, (rdf(GelarURI, rdf:type, cc:'MasterDegree'), get_id(GelarURI, ID)), IDs2),
    forall(member(ID, IDs2), assertz(degree(ID, 'M.T.'))),
    
    findall(ID, (rdf(GelarURI, rdf:type, cc:'DoctoralDegree'), get_id(GelarURI, ID)), IDs3),
    forall(member(ID, IDs3), assertz(degree(ID, 'Dr.'))),
    
    length(IDs1, C1), length(IDs2, C2), length(IDs3, C3),
    format('Loaded ~w Bachelor, ~w Master, ~w Doctoral degrees~n', [C1, C2, C3]).

opt_rdf(S, P, O, Default) :-
    ( rdf(S, P, O) -> true ; O = Default ).