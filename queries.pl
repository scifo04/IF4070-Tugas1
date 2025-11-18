:- module(queries,
    [
        courses_in_department/2,
        courses_at_level/2,
        courses_with_credits/2,
        search_course_by_name/2,
        course_info/1,
        list_all_courses/0,
        list_courses_by_department/1,
        print_statistics/0,
        total_credits/2,
        meets_credit_requirement/2
    ]).

:- use_module(rdf_loader, [course/3, degree/2]).
:- use_module(course_rules).

% ===========================================
% utility rules
% ===========================================
total_credits([], 0).
total_credits([CourseID|Rest], Total) :-
    course(CourseID, _, Credits),
    total_credits(Rest, RestTotal),
    Total is Credits + RestTotal.

meets_credit_requirement(CourseList, MinCredits) :-
    total_credits(CourseList, Total),
    Total >= MinCredits.

% ===========================================
% query rules
% ===========================================

courses_in_department(Department, Courses) :-
    findall(CourseID, course_department(CourseID, Department), Courses).

courses_at_level(LevelName, Courses) :-
    findall(CourseID, course_level(CourseID, LevelName), Courses).

courses_with_credits(Credits, Courses) :-
    findall(CourseID, course(CourseID, _, Credits), Courses).

search_course_by_name(Keyword, Results) :-
    findall((CourseID, CourseName, Credits), (
        course(CourseID, CourseName, Credits),
        sub_atom(CourseName, _, _, _, Keyword)
    ), Results).

course_info(CourseID) :-
    course(CourseID, CourseName, Credits),
    (course_level(CourseID, Level) -> true ; Level = 'Unknown'),
    (course_department(CourseID, Department) -> true ; Department = 'Unknown'),
    
    format('~nCourse ID: ~w~n', [CourseID]),
    format('Course Name: ~w~n', [CourseName]),
    format('Credits: ~w~n', [Credits]),
    format('Level (derived): ~w~n', [Level]), 
    format('Department (derived): ~w~n', [Department]), 
    
    
    (is_practical_course(CourseID) -> write('Type (derived): Practical Course\n'); true),
    (is_thesis_course(CourseID) -> write('Type (derived): Thesis/Research\n'); true),
    (is_elective_course(CourseID) -> write('Type (derived): Elective Course\n'); true),
    (is_mandatory_course(CourseID) -> write('Type (derived): Mandatory Course\n'); true).

% ===========================================
% ATURAN UTILITAS 
% ===========================================

list_all_courses :-
    write('=== All Available Courses ==='), nl,
    forall(
        course(CourseID, CourseName, Credits),
        format('~w - ~w (~w credits)~n', [CourseID, CourseName, Credits])
    ).

list_courses_by_department(Department) :-
    format('~n=== ~w Courses ===~n', [Department]),
    forall(
        (course_department(CourseID, Department),
         course(CourseID, CourseName, Credits)),
        format('~w - ~w (~w credits)~n', [CourseID, CourseName, Credits])
    ).

print_statistics :-
    findall(C, course(C, _, _), AllCourses), length(AllCourses, NumCourses),
    findall(D, degree(D, _), AllDegrees), length(AllDegrees, NumDegrees),
    
    format('~n=== Knowledge Base Statistics ===~n'),
    format('Total Courses: ~w~n', [NumCourses]),
    format('Total Degrees: ~w~n', [NumDegrees]),
    
    findall(C, is_practical_course(C), PracticalCourses), length(PracticalCourses, NumPractical),
    format('Practical Courses (derived): ~w~n', [NumPractical]).