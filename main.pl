% ===========================================
% MAIN SYSTEM (main.pl)
% ===========================================
%
% This is the main file for loading the system.
% Workflow:
% 1. Load this file: ?- [main].
% 2. Run:           ?- init.
% 3. Run:           ?- menu.
%
% ===========================================

% --- Load Core Modules ---
% Loads all system logic and rules
:- use_module(rdf_loader).
:- use_module(course_rules).
:- use_module(queries).

% ===========================================
% INITIALIZATION
% ===========================================

% This 'init' predicate MUST be run
% once to load all facts from the RDF file.
init :-
    write('Initializing system...'), nl,
    write('Loading facts from ''facts/college-course.rdf''...'), nl,
    load_rdf_facts('facts/college-course.rdf'), % Calls rdf_loader
    write('Initialization complete. System is ready.'), nl,
    write('Please run menu. to see the command list.'), nl, nl.

% ===========================================
% MAIN MENU
% ===========================================

% The 'menu' predicate replaces 'quick_start'
menu :-
    nl,
    write('╔════════════════════════════════════════════╗'), nl,
    write('║   Sistem Akademik STEI (Ontology-based)    ║'), nl,
    write('║   Quick Start Menu                         ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl,
    nl,
    write('Main Commands:'), nl,
    write('   1. init.                 - (REQUIRED) Load RDF data.'), nl,
    write('   2. menu.                 - Show this menu.'), nl,
    write('   3. list_all_courses.     - List all courses.'), nl,
    write('   4. print_statistics.     - Show statistics.'), nl,
    write('   5. course_info(''CODE'').    - Detailed course info.'), nl,
    write('   6. [test].               - Load and run the test file.'), nl,
    nl.

% ===========================================
% WELCOME MESSAGE
% ===========================================

% This message appears when [main] is loaded
:- write('System [main.pl] loaded successfully.'), nl,
   write('   ==================================='), nl,
   write('   ==> PLEASE RUN "init." NOW   <=='), nl,
   write('   ==> to load data from RDF.   <=='), nl,
   write('   ==================================='), nl, nl.