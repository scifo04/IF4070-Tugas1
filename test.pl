% ===========================================
% TEST FILE (test.pl)
% ===========================================
%
% This file contains all unit tests and scenarios
% to validate the main system.
%
% ===========================================

% --- Step 1: Load the Main System ---
% This will load main.pl and all its modules
% (rdf_loader, course_rules, queries)
:- [main].

% ===========================================
% TEST SUITE
% ===========================================

test_all :-
    nl, write('=== RUNNING ALL TESTS ==='), nl,
    
    % --- Step 2: Ensure data is initialized ---
    write('Checking data status...'), nl,
    ( \+ (current_predicate(course/3), clause(course(_,_,_),_)) ->
        write('WARNING: Data not loaded. Running init...'), nl,
        init % Call init if data is missing
    ;
        write('✓ Data is already initialized.'), nl
    ),
    nl,
    
    % --- Step 3: Run all tests ---
    test_basic_queries,
    test_course_categorization,
    test_credit_calculations,
    test_search_functions,
    
    write('=== ALL TESTS COMPLETED ==='), nl.

% ===========================================
% REMAINDER OF FILE 
% ===========================================

% Test 1: Basic Course Queries
test_basic_queries :-
    write('TEST 1: Basic Course Queries'), nl,
    write('------------------------------'), nl,
    write('1.1 Testing course retrieval...'), nl,
    (course('IF2110', Name, Credits) ->
        format('   ✓ Found course IF2110: ~w (~w credits)~n', [Name, Credits])
    ;
        write('   ✗ Failed to find course IF2110'), nl
    ),
    write('1.2 Testing course count...'), nl,
    findall(C, course(C, _, _), AllCourses),
    length(AllCourses, Count),
    format('   ✓ Total courses in database: ~w~n', [Count]),
    nl.

% Test 2: Course Categorization
test_course_categorization :-
    write('TEST 2: Course Categorization'), nl,
    write('------------------------------'), nl,
    
    write('2.1 Testing department classification (from ID)...'), nl,
    course_department('IF2110', Dept1),
    format('   ✓ IF2110 is in: ~w~n', [Dept1]),
    
    write('2.2 Testing level classification (from ID)...'), nl,
    course_level('IF2110', Level1),
    format('   ✓ IF2110 level: ~w~n', [Level1]),
    
    write('2.3 Testing practical course detection (from Name)...'), nl,
    (is_practical_course('IF4090') ->
        write('   ✓ IF4090 correctly identified as practical course'), nl
    ;
        write('   ✗ Failed to identify IF4090 as practical'), nl
    ),
    
    write('2.4 Testing thesis course detection (from Name)...'), nl,
    (is_thesis_course('EB4090') ->
        write('   ✓ EB4090 correctly identified as thesis course'), nl
    ;
        write('   ✗ Failed to identify EB4090 as thesis'), nl
    ),
    nl.

% Test 3: Credit Calculations
test_credit_calculations :-
    write('TEST 3: Credit Calculations'), nl,
    write('------------------------------'), nl,
    
    write('3.1 Testing total credits calculation...'), nl,
    total_credits(['IF2110', 'IF2120', 'IF2130'], Total),
    format('   ✓ Total credits for IF2110, IF2120, IF2130: ~w~n', [Total]),
    
    write('3.2 Testing heavy course detection...'), nl,
    (is_heavy_course('IF2150') ->
        write('   ✓ IF2150 correctly identified as heavy course'), nl
    ;
        write('   ✗ Failed to identify IF2150 as heavy'), nl
    ),
    nl.

% Test 5: Search Functions
test_search_functions :-
    write('TEST 5: Search Functions'), nl,
    write('------------------------------'), nl,
    
    write('5.1 Testing search by course name (Bhs Indonesia)...'), nl,
    search_course_by_name('Basis Data', Results),
    length(Results, NumResults),
    format('   ✓ Found ~w courses containing "Basis Data"~n', [NumResults]),
    
    write('5.2 Testing courses at level (from ID)...'), nl,
    courses_at_level('SecondYear', SecondYearCourses),
    length(SecondYearCourses, NumSecondYear),
    format('   ✓ Found ~w second year courses~n', [NumSecondYear]),
    
    write('5.3 Testing courses in department (from ID)...'), nl,
    courses_in_department('Informatics', IFCourses),
    length(IFCourses, NumBM),
    format('   ✓ Found ~w Informatics courses~n', [NumBM]),
    nl.

% ===========================================
% EXAMPLE USAGE SCENARIOS
% ===========================================

run_all_scenarios :-
    nl, write('=== RUNNING ALL USAGE SCENARIOS ==='), nl, nl,
    
    % Check data (same as in test_all)
    ( \+ (current_predicate(course/3), clause(course(_,_,_),_)) ->
        write('WARNING: Data not loaded. Running init...'), nl,
        init
    ;
        true
    ),
    
    scenario_check_graduation,
    scenario_find_by_topic,
    scenario_analyze_load,
    write('=== ALL SCENARIOS COMPLETED ==='), nl.

% --- Scenarios (Unchanged) ---
scenario_check_graduation :-
    write('=== SCENARIO: Check Graduation Requirements ==='), nl,
    write('Checking if student meets 144 credit minimum...'), nl, nl,
    
    StudentCourses = ['IF1210', 'IF1220', 'IF1230', 'IF2110', 'IF2211'],
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

scenario_find_by_topic :-
    write('=== SCENARIO: Find Courses by Topic ==='), nl,
    write('Student interested in "Jaringan" (Network)...'), nl, nl,
    
    search_course_by_name('Jaringan', Results),
    write('Found courses:'), nl,
    (Results = [] -> write('   (No courses found)'); true),
    forall(
        member((ID, Name, Credits), Results),
        format('   - ~w: ~w (~w credits)~n', [ID, Name, Credits])
    ),
    nl.

scenario_analyze_load :-
    write('=== SCENARIO: Analyze Course Load ==='), nl,
    write('Proposed semester courses: IF3110, IF3130, IF3140, IF3170'), nl, nl,
    
    ProposedCourses = ['IF3110', 'IF3130', 'IF3140', 'IF3170'],
    total_credits(ProposedCourses, Total),
    
    format('Total credits: ~w~n', [Total]),
    
    findall(C, (member(C, ProposedCourses), is_heavy_course(C)), HeavyCourses),
    length(HeavyCourses, NumHeavy),
    format('Heavy courses (4+ credits): ~w~n', [NumHeavy]),
    
    findall(C, (member(C, ProposedCourses), is_practical_course(C)), PracticalCourses),
    length(PracticalCourses, NumPractical),
    format('Practical courses: ~w~n', [NumPractical]),
    nl.