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
│   └── main.pl                     # Main system entry point    
│   └── queries.pl                  # Module for user queries
│   ├── course_rules.pl             # rdf ontology knowledge base                   
│   └── rdf_loader.pl               # module for loading rdf facts    
│   └── test.pl                     # Test
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
% 1. Load the main system
?- [main].

% 2. IMPORTANT: Initialize the database
?- init.

% 3. View the menu
?- menu.

% View all courses
?- list_all_courses.

% View statistics
?- print_statistics.

% Get course information
?- course_info('EB2101').

% Run tests (optional, loads main.pl and runs init automatically)
?- [test].
?- test_all.
```

---

## 💡 Features

### 1. Course Data Model

#### Core Facts (Loaded via `init.`)
```prolog
% Course information
course(CourseID, CourseName, Credits).
course('EB2101', 'Dasar Teknik Biomedis', 2).
course('IF2110', 'Algoritma dan Pemrograman 2', 3).

% Degree information
degree(DegreeID, DegreeName).
degree('S.T.', 'S.T.').
```

#### Sample Courses

**Biomedical Engineering (EB)** [cite: 8-30]
- EB2101 - Dasar Teknik Biomedis (2 credits)
- EB3103 - Pengolahan Sinyal Biomedis (3 credits)
- EB4090 - Tugas Akhir (4 credits)

**Informatics (IF)** [cite: 230-264]
- IF2110 - Algoritma dan Pemrograman 2 (3 credits)
- IF3170 - Inteligensi Artifisial (4 credits)
- IF4092 - Tugas Akhir (4 credits)

**Electrical Engineering (EL)** [cite: 49-84]
- EL2001 - Rangkaian Elektrik (4 credits)
- EL3010 - Pengolahan Sinyal Digital (3 credits)
- EL4091 - Tugas Akhir (3 credits)

**Graduate Courses (EI/IF)** [cite: 31-48, 265-332]
- EI7001 - Kendali Optimal & Kokoh (4 credits)
- IF5001 - Metodologi Penelitian (3 credits)

### 2. Course Categorization Rules

Rules in `course_rules.pl` derive categories by parsing Course IDs and Names.

#### Department Classification
```prolog
% Automatically detect department from course ID
course_department(CourseID, Department).

?- course_department('EB2101', Dept).
Dept = 'Biomedical Engineering'.

?- course_department('IF3170', Dept).
Dept = 'Informatics'.
```

#### Level Classification
```prolog
% Determine academic level from course ID
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
- **Masters**: Level 5, 6, or 7 courses
- **Doctoral**: Level 8 courses

#### Type Classification
```prolog
% Check if practical/lab course (from name)
is_practical_course(CourseID).
?- is_practical_course('IF4090').
true.

% Check if thesis/final project (from name)
is_thesis_course(CourseID).
?- is_thesis_course('EB4090').
true.

% Check if elective course (from name)
is_elective_course(CourseID).
?- is_elective_course('IF4085').
true.

% Check if heavy course (4+ credits)
is_heavy_course(CourseID).
?- is_heavy_course('IF3170').
true.

% Check if light course (<= 2 credits)
is_light_course(CourseID).
?- is_light_course('EB2101').
true.
```

### 3. Credit Management

```prolog
% Calculate total credits for course list
total_credits(CourseList, Total).
?- total_credits(['EB2101', 'IF2110', 'IF1210'], Total).
Total = 8.

% Check if credit requirement is met
meets_credit_requirement(CourseList, MinCredits).
?- meets_credit_requirement(['...'], 144).
true.
```

### 4. Search & Discovery

```prolog
% Search courses by name keyword
search_course_by_name(Keyword, Results).
?- search_course_by_name('Basis Data', Results).
Results = [
    ('IF2040', 'Pemodelan Basis Data', 3),
    ('IF2240', 'Basis Data', 3),
    ('IF3140', 'Sistem Basis Data', 3)
].

% Find courses by department
courses_in_department(Department, Courses).
?- courses_in_department('Informatics', Courses).

% Find courses by level
courses_at_level(Level, Courses).
?- courses_at_level('Third Year', Courses).

% Find courses with specific credits
courses_with_credits(Credits, Courses).
?- courses_with_credits(4, Courses).
```

### 5. Academic Planning

```prolog
% Get detailed course information
course_info(CourseID).
?- course_info('IF3170').

Course ID: IF3170
Course Name: Inteligensi Artifisial
Credits: 4
Level (derived): Third Year
Department (derived): Informatics
Type (derived): Mandatory Course
```

### 6. Statistics & Reporting

```prolog
% Print overall statistics
print_statistics.
?- print_statistics.

=== Knowledge Base Statistics ===
Total Courses: 349
Total Degrees: 3
Practical Courses (derived): 17

% List all courses
list_all_courses.

% List courses by department
list_courses_by_department(Department).
?- list_courses_by_department('Informatics').
```

---

## 🎯 Course ID Format

Understanding the course code structure is key, as rules depend on it.

```
XX Y ZZZ
││ │ └── Course number (001-999)
││ └──── Level (1-4: Undergrad, 5/6/7: Masters, 8: Doctoral)
│└────── Department code
└─────── Department prefix

Department Codes:
- EB = Biomedical Engineering
- EL = Electrical Engineering
- EP = Electrical Power Engineering
- ET = Telecommunication Engineering
- II = Information Systems and Technology
- IF = Informatics
- EI = Graduate Electrical Engineering
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
    ...
].
```

### Example 2: Analyze Course Load
```prolog
?- Courses = ['IF3110', 'IF3130', 'IF3140', 'IF3170'],
   total_credits(Courses, Total),
   findall(C, (member(C, Courses), is_heavy_course(C)), Heavy),
   findall(P, (member(P, Courses), is_practical_course(P)), Practical),
   length(Heavy, NumHeavy),
   length(Practical, NumPractical).
Total = 13,
NumHeavy = 1,
NumPractical = 0.
```

### Example 3: Find All Thesis Courses
```prolog
?- findall((ID, Name, Credits), 
           (is_thesis_course(ID), course(ID, Name, Credits)), 
           Theses).
Theses = [
    ('EB4090', 'Tugas Akhir', 4),
    ('EL4091', 'Tugas Akhir', 3),
    ('ET4202', 'Tugas Akhir II', 4),
    ('IF4092', 'Tugas Akhir', 4),
    ('II4092', 'Tugas Akhir', 4)
].
```

### Example 4: Count Courses by Department
```prolog
?- findall(C, course_department(C, 'Informatics'), IF),
   length(IF, IFCount),
   findall(C, course_department(C, 'Biomedical Engineering'), EB),
   length(EB, EBCount).
IFCount = 78,
EBCount = 31.
```

---

## 🧪 Testing & Validation

### Test Suite

The project includes comprehensive tests in `test.pl`:

```prolog
% Load test suite (this also loads main.pl)
?- [test].

% Run all tests (will run init automatically if needed)
?- test_all.

Individual test categories:
?- test_basic_queries.              % Basic course queries
?- test_course_categorization.      % Department/level/type
?- test_credit_calculations.        % Credit totaling
?- test_search_functions.           % Search operations
```

### Usage Scenarios

Three real-world scenarios are included:

```prolog
% Run all scenarios
?- run_all_scenarios.

% Scenario 1: Check graduation readiness
?- scenario_check_graduation.
=== SCENARIO: Check Graduation Requirements ===
Checking if student meets 144 credit minimum...
...
✗ Student needs 130 more credits

% Scenario 2: Find courses by topic
?- scenario_find_by_topic.
=== SCENARIO: Find Courses by Topic ===
Student interested in "Jaringan" (Network)...
Found courses:
   - ET2104: Jaringan Komputer (3 credits)
   - IF2230: Jaringan Komputer (3 credits)
   ...

% Scenario 3: Analyze workload
?- scenario_analyze_load.
=== SCENARIO: Analyze Course Load ===
Proposed semester courses: IF3110, IF3130, IF3140, IF3170
Total credits: 13
Heavy courses (4+ credits): 1
Practical courses: 0
```

---

## 📚 RDF Ontology Details

### Structure

The `facts/college-course.rdf` file contains:

**Classes:**
- Courses, Majors, Degree
- BacheloralDegree, MasterDegree, DoctoralDegree
- CourseCategory, GradingCategory, Semester, Year

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

### Loading RDF

The RDF data is **not** loaded automatically. You **must** run `init.` after loading `[main].` to populate the knowledge base.

```prolog
% Load the system
?- [main].

% Load the data
?- init.
```

---

## 🔧 Customization & Extension

### Adding New Courses

```prolog
% Add single course temporarily
?- assertz(course('NEW123', 'New Course Name', 4)).
```
(To add courses permanently, add them to the `college-course.rdf` file via Protégé).

### Creating Custom Rules

You can add new rules to `course_rules.pl` or `queries.pl`.

```prolog
% Find all AI-related courses
ai_courses(Courses) :-
    findall(ID, (
        course(ID, Name, _),
        (sub_atom(Name, _, _, _, 'AI');
         sub_atom(Name, _, _, _, 'Inteligensi');
         sub_atom(Name, _, _, _, 'Cerdas'))
    ), Courses).
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
?- course_info('IF2110').

% Raw course data
?- course('IF2110', Name, Credits).

% List by department
?- list_courses_by_department('Informatics').
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
   courses_in_department('Informatics', IF),
   intersection(L3, IF, Results).
```

### Categorization

```prolog
% Get level
?- course_level('IF2110', Level).

% Get department
?- course_department('IF3170', Dept).

% Check types
?- is_practical_course('IF4090').
?- is_thesis_course('IF4092').
?- is_elective_course('IF4085').
?- is_heavy_course('IF3170').
?- is_light_course('IF4090').
```

### Credits

```prolog
% Calculate total
?- total_credits(['IF2110', 'IF2120', 'IF2130'], Total).

% Check requirement
?- meets_credit_requirement(CourseList, 144).
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
?- working_directory(_, 'D:/path/to/IF4070-Tugas1').

% Reload file
?- [main].
```

**Problem**: No results from queries / `Loaded 0 courses`
```prolog
% 1. Did you run "init." first? This is required.
?- init.

% 2. Verify courses are loaded
?- listing(course).

% 3. Check namespace in rdf_loader.pl
%    It MUST match the 'xmlns:college-course'
%    tag in your college-course.rdf file.
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
