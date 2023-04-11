:- use_module(library(lists)).
:- use_module(library(strings)).
:- include('knowledgebase.pl').

%________________________________________________________________________________________________________________________________

% 1. Total Credits Requirement - 120 credits required



% total_credits_check(Courses)/1: the check for total number of credits requirement.
% Arguments:
%   Courses: The list of courses, each one a string.
%
total_credits_check(Courses) :-
	write("\n1. Total Credits (120) Requirement:\n"),
    get_total_credit(Courses, TotalCredit),
    write('The total number of credits you have is: '),
    write(TotalCredit),
    nl,
    print_total_credits_check_message(TotalCredit).


% print_total_credits_check_message(NumCredits)/1: print a message to console for the total credit requirement.
% Arguments:
%   NumCredits: An integer, the total number of credits obtained
%
print_total_credits_check_message(NumCredits) :-
	NumCredits < 120,
	Remainder is 120 - NumCredits,
	write('You still need '), 
	write(Remainder), 
	write(' number of credits to fulfill the Total Credits Requirement.\n').
print_total_credits_check_message(NumCredits) :-
	NumCredits >= 120,
	write('Congrats! You have fulfilled the Total Credits Requirement!\n').


% get_total_credit(Courses, Credits)/2: get the total number of credits for a list of courses
% Arguments:
%   Course: A list of string, each representing a course
%   Credits: An integer, the total number of credits obtained
%
get_total_credit([], 0).
get_total_credit([Course|Courses], TotalCredits) :-
    credits(Course, Credits),
    get_total_credit(Courses, RemainingCredits), 
    TotalCredits is Credits + RemainingCredits.


% credits(Course, Credits)/2: get the number of credits for a course
% Arguments:
%   Course: A string, representing a course
%   Credits: An integer, the number of credits for that course
%
credits(Course, Credits) :- 
	course(Course, Credits).
credits(_, Credits) :- 
	default_credits(Credits).


%________________________________________________________________________________________________________________________________

% 2. Communication Requirement - 6 credits from {WRDS 150; SCIE 113; any of ENGL 100, 110, 111, 112, 120, or 121}



% communication_check(Courses)/1: the check for communication requirement.
% Arguments:
%   Courses: The list of courses, each one a string.
%
communication_check(Courses) :-
	write("\n2. Communication Requirements (6 Credits) :\n"),
	setof(X, comm_course(X), Bag),
	get_subject_credits(Courses, Bag, TotalCredit),
    write('The total number of communication credits you have is: '),
    write(TotalCredit),
    nl,
    print_communication_check_message(TotalCredit).


% print_communication_check_message(NumCredits)/1: print a message to console for the communication requirement.
% Arguments:
%   NumCredits: An integer, the total number of credits obtained
%
print_communication_check_message(NumCredits) :-
	NumCredits < 6,
	Remainder is 6 - NumCredits,
	write('You still need '), 
	write(Remainder), 
	write(' number of credits to fulfill the Communication Requirement!\n').
print_communication_check_message(NumCredits) :-
	NumCredits >= 6,
	write('Congrats! You have fulfilled the Communication Requirement!\n').


%________________________________________________________________________________________________________________________________

% 3. Art Requirement - Total of 12 credits from valid Art subjects. Communication subjects can not be double counted.



% arts_check(Courses)/1: the check for arts requirement.
% Arguments:
%   Courses: The list of courses, each one a string.
%
arts_check(Courses) :-
	write("\n3. Arts Requirements (12 Credits) :\n"),
    get_arts_credits(Courses, TotalCredit),
    write("The total number of arts credits you have is: "),
    write(TotalCredit),
    nl,
    print_arts_check_message(TotalCredit).


% print_arts_check_message(NumCredits)/1: print a message to console for the arts requirement.
% Arguments:
%   NumCredits: An integer, the total number of credits obtained
%
print_arts_check_message(NumCredits) :-
	NumCredits < 12,
	Remainder is 12 - NumCredits,
	write('You still need '), 
	write(Remainder), 
	write(' number of credits to fulfill the Arts Requirement!\n').
print_arts_check_message(NumCredits) :-
	NumCredits >= 12,
	write('Congrats! You have fulfilled the Arts Requirement!\n').


% get_arts_credits(Courses, Credits)/2: Get number of art credits in courses excluding duplicate communication credits
% Arguments:
%   Courses: A list of string, each representing a course
%   Credits: An integer, the total number of credits obtained
%
get_arts_credits([], 0).
get_arts_credits(Courses, Credits) :-
	setof(X, arts_subject(X), Bag),
	count_art_subject_in_common(Courses, Bag, Count),
	setof(Y, comm_course(Y), Bag2),
	get_subject_credits(Courses, Bag2, CommCredits),
	Credits is 3 * Count - CommCredits.


% count_art_subject_in_common(Courses, Subject, Count)/2: Helper function to get_arts_credits/2. Parses the courses (e.g. "INFO 123" to "INFO" and "123") then counts the total number of courses that match with arts.
% Arguments:
%   Courses: A list of string, each representing a course
%   Subject: A list of string for arts courses, only the subject and not including the course number
%	Count: The total number of matches of courses that have same subject in the Subject list.
%
count_art_subject_in_common([], _, 0).
count_art_subject_in_common([Head|Tail], B, Count) :-
	parse_course(Head, Subject, _),
    member(Subject, B),
    count_art_subject_in_common(Tail, B, CountTail),
    Count is CountTail + 1.
count_art_subject_in_common([Head|Tail], B, Count) :-
	parse_course(Head, Subject, _),
    \+ member(Subject, B),
    count_art_subject_in_common(Tail, B, Count).

%________________________________________________________________________________________________________________________________

% 4. Required CPSC, MATH, STAT Courses
% CPSC 110, 121, 210, 221, 213, 310, 313, 320
% MATH 100, 101, 200, 221, 
% STAT 200, 302



% required_check(Courses)/1: the check for required courses of a CPSC major.
% Arguments:
%   Courses: The list of courses, each one a string.
%
required_check(Courses) :-
	write("\n4. Core CPSC Major Course Requirements:\n"),
	setof(X, required(X), Bag),
	check_all_required(Courses, Bag).


% check_all_required(Courses, RequiredCourses)/2: Iterates over each required course and checks whether it is satisfied.
% Arguments:
%   Courses: The list of courses, each one a string.
%   RequiredCourses: A list of list of strings. Each list is a required course, where any one of the courses can be used to satisfy the requirement. 
% 					(e.g. Only one of ["MATH 100", "MATH 102", "MATH 104", "MATH 180", "MATH 184", "MATH 120", "MATH 110"] is needed to satisfy this requirement)
%
check_all_required(_, []) :- !.
check_all_required(Courses, [Head | Tail]) :-
	print_required_check(Courses, Head),
	check_all_required(Courses, Tail).


% required_check(Courses, RequiredCourse)/2: Given the courses a student has taken and a required course, prints to console whether requirement is satisfied or not.
% Arguments:
%   Courses: The list of courses, each one a string.
%	RequiredCourse: A list of courses where any one can be used to satisfy the requirement.Requirements
print_required_check(Courses, Requirements) :-
	intersect_length(Courses, Requirements, Count),
	Count > 0,
	write("Congrats! You have satisfied one of "),
	write(Requirements),
	nl.
print_required_check(Courses, Requirements) :-
	intersect_length(Courses, Requirements, Count),
	Count = 0,
	write("You still need to satisfy one of "),
	write(Requirements),
	nl.


%________________________________________________________________________________________________________________________________

% 5. Upper Year CPSC Requirements - 9 credits CPSC 300+, 9 credits CPSC 400+



% upper_year_cpsc_check(Courses)/1: the check for upper-year CPSC credits.
% Arguments:
%   Courses: The list of courses, each one a string.
%
upper_year_cpsc_check(Courses) :-
	write("\n5. Upper-Year CPSC Course Requirements:\n"),
	get_300_level_cpsc_credits(Courses, Credits300Plus),
	get_400_plus_cpsc_credits(Courses, Credits400Plus),
	Temp is Credits400Plus - 9,
	Addition is max(Temp, 0),
	Credits300PlusNew is Credits300Plus + Addition,
	print_300_cpsc_check(Credits300PlusNew),
	print_400_cpsc_check(Credits400Plus).


% get_300_level_cpsc_credits(Courses, Credits)/2: Gets number of credits for CPSC courses >= 300, and < 400.
% Arguments:
%   Courses: The list of courses, each one a string.
% 	Credits: An integer, the number of credits.
%
get_300_level_cpsc_credits([], 0).
get_300_level_cpsc_credits([Head|Tail], Credits) :-
	parse_course(Head, Subject, Number),
	check_300_plus_cpsc(Subject, Number, C),
	get_300_level_cpsc_credits(Tail, RemainingCredits),
	Credits is C + RemainingCredits.


% check_300_plus_cpsc(Subject, Number, Credits)/3: Helper function for get_300_level_cpsc_credits/2
%
check_300_plus_cpsc("CPSC", Number, Credits) :-
	atom_number(Number, Atom),
	Atom >= 300,
	Atom < 400,
	Credits is 3.
check_300_plus_cpsc(_, _, 0).


% get_400_plus_cpsc_credits(Courses, Credits)/2: Gets number of credits for CPSC courses >= 400.
% Arguments:
%   Courses: The list of courses, each one a string.
% 	Credits: An integer, the number of credits.
%
get_400_plus_cpsc_credits([], 0).
get_400_plus_cpsc_credits([Head | Tail], Credits) :-
	parse_course(Head, Subject, Number),
	check_400_plus_cpsc(Subject, Number, C),
	get_400_plus_cpsc_credits(Tail, RemainingCredits),
	Credits is C + RemainingCredits.


% check_400_plus_cpsc(Subject, Number, Credits)/3: Helper function for get_400_plus_cpsc_credits/2
%
check_400_plus_cpsc("CPSC", Number, Credits) :-
	atom_number(Number, Atom),
	Atom >= 400,
	Credits is 3.
check_400_plus_cpsc(_, _, 0).


% print_300_cpsc_check(Credits)/1: print a message to console for the 300+ CPSC credits requirement.
% Arguments:
%   Credits: An integer, the total number of credits obtained
%
print_300_cpsc_check(Credits) :-
	Credits < 9,
	Remainder is 9 - Credits,
	write("Currently you have "),
	write(Credits),
	write(" credits for 300+ CPSC courses. You need "),
	write(Remainder),
	write(" more credits to satisfy the requirement.\n").
print_300_cpsc_check(Credits) :-
	Credits >= 9,
	write("Congrats! You have satisfied the 300+ CPSC credit requirement!\n").


% print_400_cpsc_check(Credits)/1: print a message to console for the 400+ CPSC credits requirement.
% Arguments:
%   Credits: An integer, the total number of credits obtained
%
print_400_cpsc_check(Credits) :-
	Credits < 9,
	Remainder is 9 - Credits,
	write("Currently you have "),
	write(Credits),
	write(" credits for 400+ CPSC courses. You need "),
	write(Remainder),
	write(" more credits to satisfy the requirement.\n").
print_400_cpsc_check(Credits) :-
	Credits >= 9,
	write("Congrats! You have satisfied the 400+ CPSC credit requirement!\n").

%________________________________________________________________________________________________________________________________

% 6. Science Breadth - 3 credits each from PHYS, BIOL, CHEM



% science_breadth_check(Courses)/1: checks the science breadth requirement.
% Arguments:
%   Courses: The list of courses, each one a string.
%
science_breadth_check(Courses) :-
	write("\n6. Science Breadth Requirement:\n"),
	get_credits_by_subject(Courses, "PHYS", PhysCredits),
	get_credits_by_subject(Courses, "BIOL", BiolCredits),
	get_credits_by_subject(Courses, "CHEM", ChemCredits),
	write("Number of physics credits is "),
	write(PhysCredits),
	print_breadth_satisfy_or_not(PhysCredits),
	write("Number of biology credits is "),
	write(BiolCredits),
	print_breadth_satisfy_or_not(BiolCredits),
	write("Number of chemisty credits is "),
	write(ChemCredits),
	print_breadth_satisfy_or_not(ChemCredits).


% print_breadth_satisfy_or_not(Credits)/1: helper function to science_breadth_check/1.
print_breadth_satisfy_or_not(Credits) :-
	Credits >= 3,
	write(". The requirement has been satified!\n").
print_breadth_satisfy_or_not(Credits) :-
	Credits < 3,
	write(". The requirement is not satified yet. You simply need 3 credits.\n").

%________________________________________________________________________________________________________________________________

% 7. Upper Year Credits - 48 credits of upper year courses



% upper_year_credit_check(Courses)/1: checks the upper year credit requirement.
% Arguments:
%   Courses: The list of courses, each one a string.
%
upper_year_credit_check(Courses) :-
	write("\n7. Upper Year Credits Requirement:\n"),
	total_upper_year_credits(Courses, Credits),
	print_upper_year_credit_check(Credits).



% print_upper_year_credit_check(Credits)/1: print a message to console for the upper-year credits requirement.
% Arguments:
%   Credits: An integer, the total number of credits obtained
%
print_upper_year_credit_check(Credits) :-
	Credits < 48,
	Remainder is 48 - Credits,
	write("Currently you have "),
	write(Credits),
	write(" credits for upper-year courses. You need "),
	write(Remainder),
	write(" more credits to satisfy the requirement of 48 credits.\n").
print_upper_year_credit_check(Credits) :-
	Credits >= 48,
	write("Congrats! You have more than 48 upper-year credits and have satisfied the requirement!\n").


% total_upper_year_credits(Courses, Credits)/2: Get number of upper-year credits in courses
% Arguments:
%   Courses: A list of string, each representing a course
%   Credits: An integer, the total number of credits obtained
%
total_upper_year_credits([], 0).
total_upper_year_credits([Head | Tail], Credits) :-
	parse_course(Head, _, Number),
	atom_number(Number, Atom),
	Atom >= 300,
	total_upper_year_credits(Tail, RemainingCredits),
	Credits is 3 + RemainingCredits.
total_upper_year_credits([Head | Tail], Credits) :-
	parse_course(Head, _, Number),
	atom_number(Number, Atom),
	Atom < 300,
	total_upper_year_credits(Tail, RemainingCredits),
	Credits is RemainingCredits.


%________________________________________________________________________________________________________________________________
% Helper Functions



% parse_course(Course, Subject, Code)/3: A course (e.g "CPSC 110") is parsed seperately into subject and code (e.g. "CPSC" and "110")
% Arguments:
%   Course: A string to be parsed.
%   Subject: The subject of the course.
%   Code: The code of the course.
parse_course(Course, Subject, Code) :-
    split_string(Course, " ", "", [Subject, Code]).    

% intersect_length(List1, List2, Length)/3: Returns the length of the intersection list between List1 and List2
% Arguments:
%   List1: First List.
%   List2: Second List.
%   Length: The length of intersection(List1, List2).
%
intersect_length([], [], 0).
intersect_length([], _, 0).
intersect_length(_, [], 0).

intersect_length([Head|Rest], List2, Length) :-
    member(Head, List2),
    intersect_length(Rest, List2, Length1),
    Length is Length1 + 1.
intersect_length([Head|Rest], List2, Length) :-
    \+ member(Head, List2),
    intersect_length(Rest, List2, Length).    


% get_subject_credits(List, SubjectList, Credits)/3: Returns the number of credits for courses in a list that match with SubjectList
% Arguments:
%   List: The list of courses.
%   SubjectList: The list of courses (number inclusive) in a subject.
%   Credits: The total number of credits.
%
get_subject_credits([], _, 0).
get_subject_credits(Courses, Bag, Credits) :-
	intersect_length(Courses, Bag, Common),
	Credits is 3 * Common.



% get_credits_by_subject(List, SubjectList, Credits)/3: Returns the number of credits for courses in a list that match with SubjectList
% Arguments:
%   List: The list of courses.
%   SubjectList: The list of courses (number exclusive) in a subject.
%   Credits: The total number of credits.
%
get_credits_by_subject([], _, 0).
get_credits_by_subject([Head|Tail], Subject, Credits) :-
	parse_course(Head, Subject, _),
	get_credits_by_subject(Tail, Subject, RemainingCredits),
	Credits is 3 + RemainingCredits.
get_credits_by_subject([Head|Tail], Subject, Credits) :-
	parse_course(Head, Subject2, _),
	not(Subject = Subject2),
	get_credits_by_subject(Tail, Subject, RemainingCredits),
	Credits is RemainingCredits.

