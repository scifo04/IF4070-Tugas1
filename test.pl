% ===========================================
% Test Queries for College Course System
% ===========================================
% This file contains example test queries to demonstrate
% the functionality of the college course rules system.
%
% To run: 
% 1. Load college_course_rules.pl first
% 2. Then run these queries
% ===========================================

% Load the main rules file
:- ['college_course_rules.pl'].

% ===========================================
% TEST SUITE
% ===========================================

test_all :-
    write('=== RUNNING ALL TESTS ==='), nl, nl,
    test_basic_queries,
    test_course_categorization,
    test_credit_calculations,
    test_prerequisites,
    test_search_functions,
    write('=== ALL TESTS COMPLETED ==='), nl.

% Test 1: Basic Course Queries
test_basic_queries :-
    write('TEST 1: Basic Course Queries'), nl,
    write('------------------------------'), nl,
    
    % Test if we can retrieve a course
    write('1.1 Testing course retrieval...'), nl,
    (course('EB2101', Name, Credits) ->
        format('  ✓ Found course EB2101: ~w (~w credits)~n', [Name, Credits])
    ;
        write('  ✗ Failed to find course EB2101'), nl
    ),
    
    % Test course count
    write('1.2 Testing course count...'), nl,
    findall(C, course(C, _, _), AllCourses),
    length(AllCourses, Count),
    format('  ✓ Total courses in database: ~w~n', [Count]),
    nl.

% Test 2: Course Categorization
test_course_categorization :-
    write('TEST 2: Course Categorization'), nl,
    write('------------------------------'), nl,
    
    % Test department classification
    write('2.1 Testing department classification...'), nl,
    course_department('EB2101', Dept1),
    format('  ✓ EB2101 is in: ~w~n', [Dept1]),
    course_department('EL2001', Dept2),
    format('  ✓ EL2001 is in: ~w~n', [Dept2]),
    
    % Test level classification
    write('2.2 Testing level classification...'), nl,
    course_level('EB2101', Level1),
    format('  ✓ EB2101 level: ~w~n', [Level1]),
    course_level('EB3103', Level2),
    format('  ✓ EB3103 level: ~w~n', [Level2]),
    
    % Test practical course detection
    write('2.3 Testing practical course detection...'), nl,
    (is_practical_course('EB2209') ->
        write('  ✓ EB2209 correctly identified as practical course'), nl
    ;
        write('  ✗ Failed to identify EB2209 as practical'), nl
    ),
    
    % Test thesis course detection
    write('2.4 Testing thesis course detection...'), nl,
    (is_thesis_course('EB4090') ->
        write('  ✓ EB4090 correctly identified as thesis course'), nl
    ;
        write('  ✗ Failed to identify EB4090 as thesis'), nl
    ),
    nl.

% Test 3: Credit Calculations
test_credit_calculations :-
    write('TEST 3: Credit Calculations'), nl,
    write('------------------------------'), nl,
    
    % Test single course credits
    write('3.1 Testing single course credits...'), nl,
    course('EB2101', _, Credits1),
    format('  ✓ EB2101 has ~w credits~n', [Credits1]),
    
    % Test total credits
    write('3.2 Testing total credits calculation...'), nl,
    total_credits(['EB2101', 'EB2102', 'EB2103'], Total),
    format('  ✓ Total credits for EB2101, EB2102, EB2103: ~w~n', [Total]),
    
    % Test heavy course
    write('3.3 Testing heavy course detection...'), nl,
    (is_heavy_course('EL2001') ->
        write('  ✓ EL2001 correctly identified as heavy course'), nl
    ;
        write('  ✗ Failed to identify EL2001 as heavy'), nl
    ),
    
    % Test light course
    write('3.4 Testing light course detection...'), nl,
    (is_light_course('EL1200') ->
        write('  ✓ EL1200 correctly identified as light course'), nl
    ;
        write('  ✗ Failed to identify EL1200 as light'), nl
    ),
    nl.

% Test 4: Prerequisites
test_prerequisites :-
    write('TEST 4: Prerequisite Checking'), nl,
    write('------------------------------'), nl,
    
    % Test prerequisite retrieval
    write('4.1 Testing prerequisite relationships...'), nl,
    findall(Pre, prerequisite('EL2001', Pre), PreReqs),
    format('  ✓ Prerequisites for EL2001: ~w~n', [PreReqs]),
    
    % Test can_take_course with prerequisites met
    write('4.2 Testing can_take_course (prerequisites met)...'), nl,
    (can_take_course('EL2001', ['EL1200']) ->
        write('  ✓ Can take EL2001 with EL1200 completed'), nl
    ;
        write('  ✗ Should be able to take EL2001'), nl
    ),
    
    % Test can_take_course without prerequisites
    write('4.3 Testing can_take_course (no prerequisites)...'), nl,
    (can_take_course('EB2101', []) ->
        write('  ✓ Can take EB2101 without prerequisites'), nl
    ;
        write('  ✗ Should be able to take EB2101'), nl
    ),
    nl.

% Test 5: Search Functions
test_search_functions :-
    write('TEST 5: Search Functions'), nl,
    write('------------------------------'), nl,
    
    % Test search by name
    write('5.1 Testing search by course name...'), nl,
    search_course_by_name('Biomedis', Results),
    length(Results, NumResults),
    format('  ✓ Found ~w courses containing "Biomedis"~n', [NumResults]),
    
    % Test courses at level
    write('5.2 Testing courses at level...'), nl,
    courses_at_level('Second Year', SecondYearCourses),
    length(SecondYearCourses, NumSecondYear),
    format('  ✓ Found ~w second year courses~n', [NumSecondYear]),
    
    % Test courses in department
    write('5.3 Testing courses in department...'), nl,
    courses_in_department('Biomedical Engineering', BMCourses),
    length(BMCourses, NumBM),
    format('  ✓ Found ~w Biomedical Engineering courses~n', [NumBM]),
    
    % Test courses with specific credits
    write('5.4 Testing courses with specific credits...'), nl,
    courses_with_credits(3, ThreeCreditCourses),
    length(ThreeCreditCourses, NumThreeCredit),
    format('  ✓ Found ~w courses with 3 credits~n', [NumThreeCredit]),
    nl.

% ===========================================
% EXAMPLE USAGE SCENARIOS
% ===========================================

% Scenario 1: Student wants to plan semester
scenario_plan_semester :-
    write('=== SCENARIO: Plan Semester ==='), nl,
    write('Student has completed: EL1200, EL2001'), nl,
    write('Looking for Third Year courses...'), nl, nl,
    
    CompletedCourses = ['EL1200', 'EL2001'],
    suggest_next_courses(CompletedCourses, 'Third Year', Suggestions),
    
    write('Suggested courses:'), nl,
    forall(
        member(CourseID, Suggestions),
        (course(CourseID, Name, Credits),
         format('  - ~w: ~w (~w credits)~n', [CourseID, Name, Credits]))
    ),
    nl.

% Scenario 2: Check graduation requirements
scenario_check_graduation :-
    write('=== SCENARIO: Check Graduation Requirements ==='), nl,
    write('Checking if student meets 144 credit minimum...'), nl, nl,
    
    % Example: student has taken these courses
    StudentCourses = ['EB2101', 'EB2102', 'EB2103', 'EB2201', 'EB2202'],
    total_credits(StudentCourses, Total),
    
    format('Courses taken: ~w~n', [StudentCourses]),
    format('Total credits earned: ~w~n', [Total]),
    
    (meets_credit_requirement(StudentCourses, 144) ->
        write('✓ Student meets graduation credit requirement'), nl
    ;
        Remaining is 144 - Total,
        format('✗ Student needs ~w more credits~n', [Remaining])
    ),
    nl.

% Scenario 3: Find courses by topic
scenario_find_by_topic :-
    write('=== SCENARIO: Find Courses by Topic ==='), nl,
    write('Student interested in "Sinyal" (Signal Processing)...'), nl, nl,
    
    search_course_by_name('Sinyal', Results),
    write('Found courses:'), nl,
    forall(
        member((ID, Name, Credits), Results),
        format('  - ~w: ~w (~w credits)~n', [ID, Name, Credits])
    ),
    nl.

% Scenario 4: Analyze course load
scenario_analyze_load :-
    write('=== SCENARIO: Analyze Course Load ==='), nl,
    write('Proposed semester courses: EB3101, EB3102, EB3103, EB3109'), nl, nl,
    
    ProposedCourses = ['EB3101', 'EB3102', 'EB3103', 'EB3109'],
    total_credits(ProposedCourses, Total),
    
    format('Total credits: ~w~n', [Total]),
    
    findall(C, (member(C, ProposedCourses), is_heavy_course(C)), HeavyCourses),
    length(HeavyCourses, NumHeavy),
    format('Heavy courses (4+ credits): ~w~n', [NumHeavy]),
    
    findall(C, (member(C, ProposedCourses), is_practical_course(C)), PracticalCourses),
    length(PracticalCourses, NumPractical),
    format('Practical courses: ~w~n', [NumPractical]),
    
    (Total >= 18 ->
        write('⚠ Warning: Heavy course load (18+ credits)'), nl
    ; Total =< 12 ->
        write('ℹ Light course load (12 or less credits)'), nl
    ;
        write('✓ Moderate course load'), nl
    ),
    nl.

% ===========================================
% RUN ALL SCENARIOS
% ===========================================

run_all_scenarios :-
    write('=== RUNNING ALL USAGE SCENARIOS ==='), nl, nl,
    scenario_plan_semester,
    scenario_check_graduation,
    scenario_find_by_topic,
    scenario_analyze_load,
    write('=== ALL SCENARIOS COMPLETED ==='), nl.

% ===========================================
% QUICK START MENU
% ===========================================

quick_start :-
    nl,
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  College Course Management System          ║'), nl,
    write('║  Quick Start Menu                          ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl,
    nl,
    write('Available commands:'), nl,
    write('  1. test_all.              - Run all tests'), nl,
    write('  2. run_all_scenarios.     - Run usage scenarios'), nl,
    write('  3. list_all_courses.      - List all courses'), nl,
    write('  4. print_statistics.      - Show statistics'), nl,
    nl.

% Display quick start on load
:- quick_start.
