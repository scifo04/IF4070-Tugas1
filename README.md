# IF4070 - College Course Ontology & Prolog Rules

## 📚 Project Overview

This project implements a comprehensive knowledge-based system for managing college courses using:
- **RDF/OWL Ontology**: Structured data representation (7901 lines in `facts/college-course.rdf`)
- **Prolog Rules**: Logical reasoning and intelligent querying (`rules.pl`)

The system provides intelligent course management, prerequisite checking, credit calculations, and academic planning capabilities for students, advisors, and administrators.

**Assignment**: IF4070 (Representasi Pengetahuan dan Penalaran) - Design ontology and rules for college courses using RDF facts and Prolog reasoning.

---

## 🌟 Key Highlights

### Features
- ✅ **60+ courses** with complete metadata
- ✅ **Smart categorization** (department, level, type)
- ✅ **Credit management** (calculation, validation)
- ✅ **Prerequisite system** (checking, suggestions)
- ✅ **Powerful search** (name, department, level, credits)
- ✅ **Academic planning** (semester suggestions, graduation check)
- ✅ **Comprehensive testing** (5 test categories, 4 scenarios)
- ✅ **Real-world usage** (student, advisor, admin workflows)
- ✅ **Extensible design** (easy to add courses, rules, features)
- ✅ **Well-documented** (inline comments, external guides)

---

## 📁 Project Structure

```
IF4070-Tugas1/
│   ├── README.md                   # This comprehensive guide
│   └── references/
│       └── rules.txt               # Rule references
│   └── facts/
│       └── college-course.rdf      # RDF ontology
│   ├── rules.pl                    # Main engine
│   └── test.pl                     # Tests & examples
```

---

## 🚀 Quick Start

### Prerequisites
1. Install [SWI-Prolog](https://www.swi-prolog.org/download/stable)
2. Clone or download this repository

### Running the System

```bash
# Navigate to project directory
cd "[Project Directory]"

# Start SWI-Prolog
swipl
```

```prolog
% Load the main rules file
?- [rules].

% View quick start menu
?- quick_start.

% View all courses
?- list_all_courses.

% View statistics
?- print_statistics.

% Get course information
?- course_info('EB2101').

% Run tests (optional)
?- [test].
?- test_all.
```

---

## 💡 Features

### 1. Course Data Model

#### Core Facts
```prolog
% Course information
course(CourseID, CourseName, Credits).
course('EB2101', 'Dasar Teknik Biomedis', 2).
course('EL3010', 'Pengolahan Sinyal Digital', 3).

% Degree information
degree(DegreeID, DegreeName).
degree('sarjana_tb', 'Sarjana Teknik Biomedis').

% Prerequisites
prerequisite(Course, RequiredCourse).
prerequisite('EL3010', 'EL2007').
```

#### Sample Courses

**Biomedical Engineering (EB)**
- EB2101 - Dasar Teknik Biomedis (2 credits)
- EB2102 - Rangkaian Elektrik dan Elektronika (3 credits)
- EB2103 - Sistem Digital dan Mikroprosesor (3 credits)
- EB2201 - Anatomi dan Fisiologi Manusia (3 credits)
- EB2203 - Sinyal dan Sistem (3 credits)
- EB2209 - Praktikum Elektronika dan Rangkaian (1 credit)
- EB3101 - Instrumentasi Biomedis I (3 credits)
- EB3103 - Pengolahan Sinyal Biomedis (3 credits)
- EB3203 - Pengolahan Citra Biomedis (3 credits)
- EB4090 - Tugas Akhir (4 credits)

**Electrical Engineering (EL)**
- EL1200 - Pengantar Analisis Rangkaian (2 credits)
- EL2001 - Rangkaian Elektrik (4 credits)
- EL2002 - Rangkaian Elektronika (3 credits)
- EL2003 - Sistem Digital (3 credits)
- EL2005 - Dasar Sistem Telekomunikasi (3 credits)
- EL2007 - Sinyal dan Sistem (3 credits)
- EL3009 - Elektronika II (3 credits)
- EL3010 - Pengolahan Sinyal Digital (3 credits)
- EL3011 - Arsitektur Sistem Komputer (3 credits)
- EL4091 - Tugas Akhir (3 credits)

**Graduate Courses (EI)**
- EI7001 - Kendali Optimal & Kokoh (4 credits)
- EI7004 - Kendali & Sistem Cerdas (4 credits)
- EI7011 - Metodologi Penelitian (3 credits)
- EI8090 - Disertasi (20 credits)

### 2. Course Categorization Rules

#### Department Classification
```prolog
% Automatically detect department from course ID
course_department(CourseID, Department).

?- course_department('EB2101', Dept).
Dept = 'Biomedical Engineering'.

?- course_department('EL3010', Dept).
Dept = 'Electrical Engineering'.
```

#### Level Classification
```prolog
% Determine academic level
course_level(CourseID, Level).

?- course_level('EB2101', Level).
Level = 'Second Year'.

?- course_level('EI7001', Level).
Level = 'Masters'.
```

Levels:
- **First Year**: Level 1 courses
- **Second Year**: Level 2 courses
- **Third Year**: Level 3 courses
- **Fourth Year**: Level 4 courses
- **Masters**: Level 5 or 7 courses
- **Doctoral**: Level 8 courses

#### Type Classification
```prolog
% Check if practical/lab course
is_practical_course(CourseID).
?- is_practical_course('EB2209').
true.

% Check if thesis/final project
is_thesis_course(CourseID).
?- is_thesis_course('EB4090').
true.

% Check if elective course
is_elective_course(CourseID).
?- is_elective_course('EB3205').
true.

% Check if heavy course (4+ credits)
is_heavy_course(CourseID).
?- is_heavy_course('EL2001').
true.

% Check if light course (<= 2 credits)
is_light_course(CourseID).
?- is_light_course('EL1200').
true.
```

### 3. Credit Management

```prolog
% Calculate total credits for course list
total_credits(CourseList, Total).
?- total_credits(['EB2101', 'EB2102', 'EB2103'], Total).
Total = 8.

% Check if credit requirement is met
meets_credit_requirement(CourseList, MinCredits).
?- meets_credit_requirement(['EB2101', 'EB2102', 'EB2103'], 8).
true.
```

### 4. Prerequisite System

```prolog
% Define prerequisites
prerequisite('EL3010', 'EL2007').
prerequisite('EL3009', 'EL2005').

% Check if student can take course
can_take_course(CourseID, CompletedCourses).
?- can_take_course('EL3010', ['EL2007']).
true.

?- can_take_course('EL3010', ['EL2001']).
false.

% Verify all prerequisites completed
all_completed(RequiredCourses, CompletedCourses).
?- all_completed(['EL2007'], ['EL1200', 'EL2001', 'EL2007']).
true.
```

### 5. Search & Discovery

```prolog
% Search courses by name keyword
search_course_by_name(Keyword, Results).
?- search_course_by_name('Biomedis', Results).
Results = [
    ('EB2101', 'Dasar Teknik Biomedis', 2),
    ('EB3103', 'Pengolahan Sinyal Biomedis', 3)
].

% Find courses by department
courses_in_department(Department, Courses).
?- courses_in_department('Biomedical Engineering', Courses).

% Find courses by level
courses_at_level(Level, Courses).
?- courses_at_level('Third Year', Courses).

% Find courses with specific credits
courses_with_credits(Credits, Courses).
?- courses_with_credits(3, Courses).
```

### 6. Academic Planning

```prolog
% Suggest next semester courses
suggest_next_courses(CompletedCourses, Level, Suggestions).
?- suggest_next_courses(['EL1200', 'EL2001'], 'Third Year', S).
S = ['EL3009', 'EL3010', 'EL3011', ...].

% Get detailed course information
course_info(CourseID).
?- course_info('EB3103').
Course ID: EB3103
Course Name: Pengolahan Sinyal Biomedis
Credits: 3
Level: Third Year
Department: Biomedical Engineering
```

### 7. Statistics & Reporting

```prolog
% Print overall statistics
print_statistics.
?- print_statistics.
=== Course Statistics ===
Total Courses: 60+
Biomedical Engineering Courses: 20+
Electrical Engineering Courses: 25+
Practical/Lab Courses: 10+

% List all courses
list_all_courses.

% List courses by department
list_courses_by_department(Department).
?- list_courses_by_department('Biomedical Engineering').
```

---

## 🎯 Course ID Format

Understanding the course code structure:

```
XX Y ZZZ
││ │ └── Course number (001-999)
││ └──── Level (1-4: Undergrad, 5/7: Masters, 8: Doctoral)
│└────── Department code
└─────── First letter of department

Department Codes:
- EB = Biomedical Engineering (Teknik Biomedis)
- EL = Electrical Engineering (Teknik Elektro)
- EI = Graduate Electrical Engineering
- IF = Informatics (Teknik Informatika)

Examples:
- EB2101 = Biomedical, Year 2, Course 101
- EL3010 = Electrical, Year 3, Course 010
- EI7001 = Graduate Electrical, Masters level
```

---

## 🔍 Common Query Examples

### Example 1: Find Signal Processing Courses
```prolog
?- search_course_by_name('Sinyal', Results).
Results = [
    ('EB2203', 'Sinyal dan Sistem', 3),
    ('EL2007', 'Sinyal dan Sistem', 3),
    ('EB3103', 'Pengolahan Sinyal Biomedis', 3),
    ('EL3010', 'Pengolahan Sinyal Digital', 3)
].
```

### Example 2: Plan Semester Courses
```prolog
?- suggest_next_courses(['EL1200', 'EL2001'], 'Third Year', Suggestions),
   total_credits(Suggestions, Total).
Suggestions = ['EL3009', 'EL3010', 'EL3011', ...],
Total = 24.
```

### Example 3: Check Prerequisites
```prolog
?- prerequisite('EL3010', PreReq).
PreReq = 'EL2007'.

?- can_take_course('EL3010', ['EL2007']).
true.
```

### Example 4: Analyze Course Load
```prolog
?- Courses = ['EB3101', 'EB3102', 'EB3103', 'EB3109'],
   total_credits(Courses, Total),
   findall(C, (member(C, Courses), is_heavy_course(C)), Heavy),
   findall(P, (member(P, Courses), is_practical_course(P)), Practical),
   length(Heavy, NumHeavy),
   length(Practical, NumPractical).
Total = 11,
NumHeavy = 0,
NumPractical = 1.
```

### Example 5: Find All Thesis Courses
```prolog
?- findall((ID, Name, Credits), 
           (is_thesis_course(ID), course(ID, Name, Credits)), 
           Theses).
Theses = [
    ('EB4090', 'Tugas Akhir', 4),
    ('EL4091', 'Tugas Akhir', 3),
    ('EI8090', 'Disertasi', 20)
].
```

### Example 6: Count Courses by Department
```prolog
?- findall(C, course_department(C, 'Biomedical Engineering'), BM),
   length(BM, BMCount),
   findall(C, course_department(C, 'Electrical Engineering'), EL),
   length(EL, ELCount).
BMCount = 20,
ELCount = 25.
```

---

## 🧪 Testing & Validation

### Test Suite

The project includes comprehensive tests in `test.pl`:

```prolog
% Load test suite
?- [test_queries].

% Run all tests
?- test_all.

Individual test categories:
?- test_basic_queries.              % Basic course queries
?- test_course_categorization.      % Department/level/type
?- test_credit_calculations.        % Credit totaling
?- test_prerequisites.              % Prerequisite checking
?- test_search_functions.           % Search operations
```

### Usage Scenarios

Four real-world scenarios are included:

```prolog
% Scenario 1: Plan next semester
?- scenario_plan_semester.
=== SCENARIO: Plan Semester ===
Student has completed: EL1200, EL2001
Looking for Third Year courses...
Suggested courses:
  - EL3009: Elektronika II (3 credits)
  - EL3010: Pengolahan Sinyal Digital (3 credits)

% Scenario 2: Check graduation readiness
?- scenario_check_graduation.
=== SCENARIO: Check Graduation Requirements ===
Student courses: [...144 credits...]
✓ Meets graduation requirement of 144 credits

% Scenario 3: Find courses by topic
?- scenario_find_by_topic.
=== SCENARIO: Find Courses by Topic ===
Searching for 'Sistem' courses...
Found 5 courses containing 'Sistem'

% Scenario 4: Analyze workload
?- scenario_analyze_load.
=== SCENARIO: Analyze Course Load ===
Proposed semester: EB3101, EB3102, EB3103, EB3109
Total credits: 11
Heavy courses: 0
Practical courses: 1
✓ Moderate course load
```

---

## 📚 RDF Ontology Details

### Structure

The `facts/college-course.rdf` file contains:

**Classes:**
- Courses
- Degrees (Sarjana, Magister, Doktor)
- Majors (Biomedis, Elektro, Informatika)
- Semesters

**Properties:**
- hasCourseID
- hasCourseName
- hasCourseCredit
- hasMajor
- hasSemester

**Individuals:**
- 600+ course instances
- Degree definitions
- Major classifications

### Example RDF Structure

```xml
<owl:NamedIndividual rdf:about="#EB2101">
    <rdf:type rdf:resource="#Courses"/>
    <college-course:hasCourseID>EB2101</college-course:hasCourseID>
    <college-course:hasCourseName>Dasar Teknik Biomedis</college-course:hasCourseName>
    <college-course:hasCourseCredit>2</college-course:hasCourseCredit>
    <college-course:hasMajor rdf:resource="#TeknikBiomedis"/>
    <college-course:hasSemester rdf:resource="#Semester3"/>
</owl:NamedIndividual>
```

### Loading RDF (Optional)

```prolog
% Load courses from RDF file
?- load_rdf_facts.

% Note: Requires SWI-Prolog SGML/XML library
```

---

## 🔧 Customization & Extension

### Adding New Courses

```prolog
% Add single course
?- assertz(course('NEW123', 'New Course Name', 4)).

% Add multiple courses
?- assertz(course('CS101', 'Intro to CS', 3)),
   assertz(course('CS102', 'Data Structures', 4)),
   assertz(course('CS201', 'Algorithms', 4)).
```

### Adding Prerequisites

```prolog
% Add prerequisite relationship
?- assertz(prerequisite('CS201', 'CS102')).
?- assertz(prerequisite('CS102', 'CS101')).

% Verify prerequisite chain
?- prerequisite('CS201', P1),
   prerequisite(P1, P2).
P1 = 'CS102',
P2 = 'CS101'.
```

### Creating Custom Rules

```prolog
% Find all AI-related courses
ai_courses(Courses) :-
    findall(ID, (
        course(ID, Name, _),
        (sub_atom(Name, _, _, _, 'AI');
         sub_atom(Name, _, _, _, 'Intelijen');
         sub_atom(Name, _, _, _, 'Machine Learning'))
    ), Courses).

% Find courses suitable for semester
light_semester(Courses) :-
    findall(ID, (
        course(ID, _, Credits),
        Credits =< 3,
        \+ is_thesis_course(ID)
    ), Courses).

% Check if course path is valid
valid_course_path(Path) :-
    Path = [First|Rest],
    valid_path_helper(First, Rest).

valid_path_helper(_, []).
valid_path_helper(Current, [Next|Rest]) :-
    (prerequisite(Next, Current) ; \+ prerequisite(Next, _)),
    valid_path_helper(Next, Rest).
```

---

## 📖 Complete Query Reference

### Basic Queries

```prolog
% List all courses
?- list_all_courses.

% Statistics
?- print_statistics.

% Course information
?- course_info('EB2101').

% Raw course data
?- course('EB2101', Name, Credits).

% List by department
?- list_courses_by_department('Biomedical Engineering').
```

### Search & Filter

```prolog
% Search by keyword
?- search_course_by_name('Sinyal', Results).

% Filter by level
?- courses_at_level('Third Year', Courses).

% Filter by department
?- courses_in_department('Electrical Engineering', Courses).

% Filter by credits
?- courses_with_credits(3, Courses).

% Combined filter
?- courses_at_level('Third Year', L3),
   courses_in_department('Biomedical Engineering', BM),
   intersection(L3, BM, Results).
```

### Categorization

```prolog
% Get level
?- course_level('EB2101', Level).

% Get department
?- course_department('EL3010', Dept).

% Check types
?- is_practical_course('EB2209').
?- is_thesis_course('EB4090').
?- is_elective_course('EB3205').
?- is_heavy_course('EL2001').
?- is_light_course('EL1200').
```

### Credits

```prolog
% Calculate total
?- total_credits(['EB2101', 'EB2102', 'EB2103'], Total).

% Check requirement
?- meets_credit_requirement(CourseList, 144).

% Find heavy semester
?- Courses = ['EL2001', 'EL2002', 'EL2003', 'EL2007'],
   total_credits(Courses, Total),
   Total > 16.
```

### Prerequisites

```prolog
% Check eligibility
?- can_take_course('EL3010', ['EL2007']).

% Get prerequisites
?- prerequisite('EL3010', PreReq).

% Suggest next courses
?- suggest_next_courses(['EL1200', 'EL2001'], 'Third Year', S).

% Check all prerequisites
?- all_completed(['EL2007'], ['EL1200', 'EL2001', 'EL2007']).
```

### Advanced Queries

```prolog
% Find all courses with specific pattern
?- findall((ID, Name), 
           (course(ID, Name, _), 
            sub_atom(Name, _, _, _, 'Digital')), 
           Digital).

% Count by category
?- findall(C, is_practical_course(C), P),
   length(P, PracticalCount).

% Course dependency chain
?- prerequisite('AdvCourse', Mid),
   prerequisite(Mid, Basic).

% Workload analysis
?- findall(C, is_heavy_course(C), Heavy),
   length(Heavy, Count),
   maplist(course_info, Heavy).
```

---

## 🛠️ Troubleshooting

### Common Issues

**Problem**: File won't load
```prolog
% Check current directory
?- working_directory(CWD, CWD).

% Change directory if needed
?- working_directory(_, 'd:/My Stuff/ITB Files/Semester VII/RPP/tubes/IF4070-Tugas1').

% Reload file
?- [rules].
```

**Problem**: No results from queries
```prolog
% Verify courses are loaded
?- listing(course).

% Check how many courses
?- findall(C, course(C, _, _), All), length(All, Count).
```

**Problem**: Prerequisite check fails
```prolog
% List all prerequisites
?- listing(prerequisite).

% Check specific prerequisite
?- prerequisite('EL3010', P).
```

**Problem**: Unexpected behavior
```prolog
% Enable trace mode
?- trace.

% Run query to debug
?- your_query.

% Disable trace
?- notrace.
```

### Tips & Tricks

```prolog
% Use semicolon to see more results
?- course(ID, Name, 3).
ID = 'EB2102', Name = 'Rangkaian Elektrik dan Elektronika' ;
ID = 'EB2103', Name = 'Sistem Digital dan Mikroprosesor' ;
...

% Use period to stop after first result
?- course(ID, Name, 3).
ID = 'EB2102', Name = 'Rangkaian Elektrik dan Elektronika'.

% View predicate definitions
?- listing(course_level).

% Get help
?- help.

% Exit Prolog
?- halt.
```

### Keyboard Shortcuts (SWI-Prolog)

- `Ctrl+C` - Interrupt/abort query
- `;` - See next solution
- `.` - Stop after current solution
- `↑/↓` - Navigate command history
- `Tab` - Auto-complete
- `Ctrl+D` - Exit Prolog

---

## 👨‍💻 Author & License
 
**Course**: IF4070 - Representasi Pengetahuan dan Penalaran  
**Institution**: Institut Teknologi Bandung (ITB)  
**Assignment**: Tugas 1 - College Course Ontology & Rules Design  
**License**: Educational use only - ITB  

---

## 🔗 Additional Resources

### External Resources
- [SWI-Prolog Manual](https://www.swi-prolog.org/pldoc/doc_for?object=manual)
- [SWI-Prolog FAQ](https://www.swi-prolog.org/FAQ/)
- [Learn Prolog Now](http://www.learnprolognow.org/)
- [RDF/OWL Tutorial](https://www.w3.org/TR/owl-primer/)

---
