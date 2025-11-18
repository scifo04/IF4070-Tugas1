:- module(course_rules,
    [
        course_level/2,           
        course_department/2,      
        is_practical_course/1,    
        is_thesis_course/1,       
        is_elective_course/1,     
        is_mandatory_course/1,
        is_undergraduate_course/1,
        is_graduate_course/1,
        is_heavy_course/1,
        is_light_course/1
    ]).

% rdf loader
:- use_module(rdf_loader, [course/3]). 

% ===========================================
% Categorization rulkes
% ===========================================

course_level(CourseID, Level) :-
    course(CourseID, _, _),
    atom_chars(CourseID, [_, _, LevelChar|_]),
    (   LevelChar = '1' -> Level = 'First Year';
        LevelChar = '2' -> Level = 'Second Year';
        LevelChar = '3' -> Level = 'Third Year';
        LevelChar = '4' -> Level = 'Fourth Year';
        LevelChar = '5' -> Level = 'Masters';
        LevelChar = '6' -> Level = 'Masters'; 
        LevelChar = '7' -> Level = 'Masters';
        LevelChar = '8' -> Level = 'Doctoral';
        Level = 'Unknown'
    ).

course_department(CourseID, Department) :-
    course(CourseID, _, _),
    atom_chars(CourseID, [D1, D2|_]),
    atom_chars(DeptCode, [D1, D2]),
    (   DeptCode = 'EB' -> Department = 'Biomedical Engineering';
        DeptCode = 'EL' -> Department = 'Electrical Engineering';
        DeptCode = 'EP' -> Department = 'Electrical Power Engineering';
        DeptCode = 'II' -> Department = 'Information Systems and Technology';
        DeptCode = 'IF' -> Department = 'Informatics';
        DeptCode = 'EI' -> Department = 'Graduate Electrical Engineering';
        DeptCode = 'ET' -> Department = 'Telecommunication Engineering';
        Department = 'Unknown'
    ).

is_practical_course(CourseID) :-
    course(CourseID, CourseName, _),
    (   sub_atom(CourseName, _, _, _, 'Praktikum'); 
        sub_atom(CourseName, _, _, _, 'Proyek'); 
        sub_atom(CourseName, _, _, _, 'Kerja Praktik'); 
        sub_atom(CourseName, _, _, _, 'Laboratorium') 
    ).

is_thesis_course(CourseID) :-
    course(CourseID, CourseName, _),
    (   sub_atom(CourseName, _, _, _, 'Tugas Akhir'); 
        sub_atom(CourseName, _, _, _, 'Disertasi'); 
        sub_atom(CourseName, _, _, _, 'Tesis') 
    ).

is_elective_course(CourseID) :-
    course(CourseID, CourseName, _),
    (   sub_atom(CourseName, _, _, _, 'Topik Khusus'); 
        sub_atom(CourseName, _, _, _, 'Pilihan'); 
        sub_atom(CourseName, _, _, _, 'Studi Mandiri') 
    ).

is_mandatory_course(CourseID) :-
    course(CourseID, _, _),
    \+ is_elective_course(CourseID),
    \+ is_thesis_course(CourseID),
    \+ is_practical_course(CourseID).

is_undergraduate_course(CourseID) :-
    course_level(CourseID, Level),
    member(Level, ['First Year', 'Second Year', 'Third Year', 'Fourth Year']).

is_graduate_course(CourseID) :-
    course_level(CourseID, Level),
    member(Level, ['Masters', 'Doctoral']).

% ===========================================
% Credits Rules
% ===========================================

is_heavy_course(CourseID) :-
    course(CourseID, _, Credits),
    Credits >= 4.

is_light_course(CourseID) :-
    course(CourseID, _, Credits),
    Credits =< 2.