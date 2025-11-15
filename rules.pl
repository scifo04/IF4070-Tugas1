% ===========================================
% College Course Ontology - Prolog Rules
% ===========================================
% Description: Rules and queries for college course management system
% ===========================================

% TODO: 
% - major, degree_type, semester, and grading is still undefined
% - add more prerequisite rules or load from RDF
% - enhance course categorization rules
% - add rules about major, degree_type, semester, grading
% - refactor this code to be modular

% Load the XML parser library
:- use_module(library(sgml)).
:- use_module(library(xpath)).

% Dynamic predicates for storing facts
:- dynamic course/3.           % course(ID, Name, Credits)
:- dynamic degree/2.           % degree(ID, Name)
:- dynamic degree_type/2.      % degree_type(ID, Type)
:- dynamic major/2.            % major(ID, Name)
:- dynamic semester/1.         % semester(Type)
:- dynamic grading/1.          % grading(Type)

% ===========================================
% LOAD RDF FILE AND EXTRACT FACTS
% ===========================================

% Load and parse the RDF file
load_rdf_facts :-
    retractall(course(_, _, _)),
    retractall(degree(_, _)),
    retractall(degree_type(_, _)),
    retractall(major(_, _)),
    retractall(semester(_)),
    retractall(grading(_)),
    write('Loading RDF file...'), nl,
    load_xml('facts/college-course.rdf', DOM, []),
    write('XML loaded successfully'), nl,
    extract_all_courses(DOM),
    extract_all_degrees(DOM),
    write('RDF facts loaded successfully!'), nl,
    nl,
    course_statistics.

% Extract course information
extract_all_courses(DOM) :-
    write('Extracting courses...'), nl,
    findall(course(ID, Name, Credits),
            extract_single_course(DOM, ID, Name, Credits),
            Courses),
    length(Courses, Count),
    forall(member(course(ID, Name, Credits), Courses),
           assertz(course(ID, Name, Credits))),
    format('Loaded ~w courses~n', [Count]).

extract_single_course(DOM, CourseID, CourseName, Credits) :-
    % Approach 1: DOM is a list with one element
    (DOM = [RootElement] -> 
        Root = RootElement 
    ; 
        Root = DOM
    ),
    
    % Extract the root element
    Root = element(_RootName, _RootAttrs, Children),
    
    % Find NamedIndividual elements
    member(Individual, Children),
    Individual = element(IndivTag, IndivAttrs, Properties),
    
    % Check if its a NamedIndividual (handle different namespace formats)
    (IndivTag = 'owl:NamedIndividual' ; IndivTag = 'NamedIndividual'),
    
    % Get the rdf:about attribute
    (member('rdf:about'=AboutURI, IndivAttrs) ; member(about=AboutURI, IndivAttrs)),
    
    % Check if its a Course type
    member(TypeElement, Properties),
    (TypeElement = element('rdf:type', TypeAttrs, _) ; TypeElement = element(type, TypeAttrs, _)),
    (member('rdf:resource'=TypeURI, TypeAttrs) ; member(resource=TypeURI, TypeAttrs)),
    atom_string(TypeURI, TypeStr),
    sub_string(TypeStr, _, _, _, "Courses"),
    
    % Extract Course ID from URI
    atom_string(AboutURI, AboutStr),
    split_string(AboutStr, "#", "", [_, IDStr]),
    atom_string(CourseID, IDStr),
    
    % Extract course name
    find_property_value(Properties, 'hasCourseName', CourseName),
    
    % Extract credits
    find_property_value(Properties, 'hasCourseCredit', CreditText),
    atom_number(CreditText, Credits).

find_property_value(Properties, PropNamePart, Value) :-
    member(element(FullPropName, _, [TextValue]), Properties),
    atom_string(FullPropName, PropNameStr),
    sub_string(PropNameStr, _, _, _, PropNamePart),
    atom_string(Value, TextValue).

% Extract degree information
extract_all_degrees(DOM) :-
    write('Extracting degrees...'), nl,
    findall(degree(ID, Name),
            extract_single_degree(DOM, ID, Name),
            Degrees),
    length(Degrees, Count),
    forall(member(degree(ID, Name), Degrees),
           assertz(degree(ID, Name))),
    format('Loaded ~w degrees~n', [Count]).


extract_single_degree(DOM, DegreeID, DegreeName) :-
    (DOM = [RootElement] -> Root = RootElement ; Root = DOM),
    Root = element(_RootName, _RootAttrs, Children),
    member(Individual, Children),
    Individual = element(IndivTag, IndivAttrs, Properties),
    (IndivTag = 'owl:NamedIndividual' ; IndivTag = 'NamedIndividual'),
    (member('rdf:about'=AboutURI, IndivAttrs) ; member(about=AboutURI, IndivAttrs)),
    member(TypeElement, Properties),
    (TypeElement = element('rdf:type', TypeAttrs, _) ; TypeElement = element(type, TypeAttrs, _)),
    (member('rdf:resource'=TypeURI, TypeAttrs) ; member(resource=TypeURI, TypeAttrs)),
    atom_string(TypeURI, TypeStr),
    (sub_string(TypeStr, _, _, _, "Degree") ; sub_string(TypeStr, _, _, _, "Major")),
    atom_string(AboutURI, AboutStr),
    split_string(AboutStr, "#", "", [_, IDStr]),
    atom_string(DegreeID, IDStr),
    (find_property_value(Properties, 'hasDegreeName', DegreeName) ;
     find_property_value(Properties, 'hasMajorName', DegreeName)).

% Display statistics
course_statistics :-
    findall(_, course(_, _, _), Courses),
    findall(_, degree(_, _), Degrees),
    length(Courses, NumCourses),
    length(Degrees, NumDegrees),
    format('Total courses loaded: ~w~n', [NumCourses]),
    format('Total degrees loaded: ~w~n', [NumDegrees]).

% ===========================================
% COURSE CATEGORIZATION RULES
% ===========================================

% Identify course level based on course ID
course_level(CourseID, Level) :-
    course(CourseID, _, _),
    atom_chars(CourseID, [_, _, LevelChar|_]),
    (
        LevelChar = '1' -> Level = 'First Year';
        LevelChar = '2' -> Level = 'Second Year';
        LevelChar = '3' -> Level = 'Third Year';
        LevelChar = '4' -> Level = 'Fourth Year';
        LevelChar = '5' -> Level = 'Masters';
        LevelChar = '7' -> Level = 'Masters';
        LevelChar = '8' -> Level = 'Doctoral';
        Level = 'Unknown'
    ).

% Identify course department
course_department(CourseID, Department) :-
    course(CourseID, _, _),
    atom_chars(CourseID, [D1, D2|_]),
    atom_chars(DeptCode, [D1, D2]),
    (
        DeptCode = 'EB' -> Department = 'Biomedical Engineering';
        DeptCode = 'EL' -> Department = 'Electrical Engineering';
        DeptCode = 'EP' -> Department = 'Electrical Power Engineering';
        DeptCode = 'II' -> Department = 'Information Systems and Technology';
        DeptCode = 'IF' -> Department = 'Informatics';
        DeptCode = 'EI' -> Department = 'Graduate Electrical Engineering';
        Department = 'Unknown'
    ).

% Check if course is practical/lab
is_practical_course(CourseID) :-
    course(CourseID, CourseName, _),
    (
        sub_atom(CourseName, _, _, _, 'Praktikum');
        sub_atom(CourseName, _, _, _, 'Proyek');
        sub_atom(CourseName, _, _, _, 'Kerja Praktik')
    ).

% Check if course is thesis/final project
is_thesis_course(CourseID) :-
    course(CourseID, CourseName, _),
    (
        sub_atom(CourseName, _, _, _, 'Tugas Akhir');
        sub_atom(CourseName, _, _, _, 'Disertasi');
        sub_atom(CourseName, _, _, _, 'Tesis')
    ).

% Check if course is elective
is_elective_course(CourseID) :-
    course(CourseID, CourseName, _),
    (
        sub_atom(CourseName, _, _, _, 'Topik Khusus');
        sub_atom(CourseName, _, _, _, 'Pilihan');
        sub_atom(CourseName, _, _, _, 'Studi Mandiri')
    ).

% ===========================================
% CREDIT CALCULATION RULES
% ===========================================

% Calculate total credits for a list of courses
total_credits([], 0).
total_credits([CourseID|Rest], Total) :-
    course(CourseID, _, Credits),
    total_credits(Rest, RestTotal),
    Total is Credits + RestTotal.

% Check if course is heavy (>= 4 credits)
is_heavy_course(CourseID) :-
    course(CourseID, _, Credits),
    Credits >= 4.

% Check if course is light (<= 2 credits)
is_light_course(CourseID) :-
    course(CourseID, _, Credits),
    Credits =< 2.

% ===========================================
% PREREQUISITE RULES (TODO: add more later or load from RDF)
% ===========================================

% Define prerequisite relationships
% prerequisite(Course, RequiredCourse)
prerequisite('EL2001', 'EL1200').
prerequisite('EL3009', 'EL2005').
prerequisite('EL3010', 'EL2007').
prerequisite('EL3011', 'EL2002').
prerequisite('EL3012', 'EL2002').
prerequisite('EB3103', 'EB2203').
prerequisite('EB3203', 'EB2203').

% Check if student can take a course (has all prerequisites)
can_take_course(CourseID, CompletedCourses) :-
    course(CourseID, _, _),
    findall(PreReq, prerequisite(CourseID, PreReq), Prerequisites),
    all_completed(Prerequisites, CompletedCourses).

% Helper predicate to check if all prerequisites are completed
all_completed([], _).
all_completed([PreReq|Rest], CompletedCourses) :-
    member(PreReq, CompletedCourses),
    all_completed(Rest, CompletedCourses).

% ===========================================
% QUERY RULES
% ===========================================

% Find all courses in a department
courses_in_department(Department, Courses) :-
    findall(CourseID, (
        course_department(CourseID, Department)
    ), Courses).

% Find all courses at a specific level
courses_at_level(Level, Courses) :-
    findall(CourseID, (
        course_level(CourseID, Level)
    ), Courses).

% Find courses with specific credit amount
courses_with_credits(Credits, Courses) :-
    findall(CourseID, (
        course(CourseID, _, Credits)
    ), Courses).

% Search courses by name (partial match)
search_course_by_name(Keyword, Results) :-
    findall((CourseID, CourseName, Credits), (
        course(CourseID, CourseName, Credits),
        sub_atom(CourseName, _, _, _, Keyword)
    ), Results).

% Get course details
course_info(CourseID) :-
    course(CourseID, CourseName, Credits),
    course_level(CourseID, Level),
    course_department(CourseID, Department),
    format('~nCourse ID: ~w~n', [CourseID]),
    format('Course Name: ~w~n', [CourseName]),
    format('Credits: ~w~n', [Credits]),
    format('Level: ~w~n', [Level]),
    format('Department: ~w~n', [Department]),
    (is_practical_course(CourseID) -> write('Type: Practical/Lab Course\n'); true),
    (is_thesis_course(CourseID) -> write('Type: Thesis/Final Project\n'); true),
    (is_elective_course(CourseID) -> write('Type: Elective Course\n'); true).

% ===========================================
% DEGREE PLANNING RULES
% ===========================================

% Calculate if credit requirements are met
meets_credit_requirement(CourseList, MinCredits) :-
    total_credits(CourseList, Total),
    Total >= MinCredits.

% Suggest courses for next semester
suggest_next_courses(CompletedCourses, Level, Suggestions) :-
    findall(CourseID, (
        course_level(CourseID, Level),
        can_take_course(CourseID, CompletedCourses),
        \+ member(CourseID, CompletedCourses)
    ), Suggestions).

% ===========================================
% UTILITY RULES
% ===========================================

% List all available courses
list_all_courses :-
    write('=== All Available Courses ==='), nl,
    forall(
        course(CourseID, CourseName, Credits),
        format('~w - ~w (~w credits)~n', [CourseID, CourseName, Credits])
    ).

% List courses by department
list_courses_by_department(Department) :-
    format('~n=== ~w Courses ===~n', [Department]),
    forall(
        (course_department(CourseID, Department),
         course(CourseID, CourseName, Credits)),
        format('~w - ~w (~w credits)~n', [CourseID, CourseName, Credits])
    ).

% Statistics
print_statistics :-
    findall(C, course(C, _, _), AllCourses),
    length(AllCourses, TotalCourses),
    findall(C, is_practical_course(C), PracticalCourses),
    length(PracticalCourses, NumPractical),
    format('~n=== Course Statistics ===~n'),
    format('Total Courses: ~w~n', [TotalCourses]),
    format('Practical/Lab Courses: ~w~n', [NumPractical]).
