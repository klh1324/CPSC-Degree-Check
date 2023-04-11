:- use_module(library(lists)).
:- include('knowledgebase.pl').

%________________________________________________________________________________________________________________________________

% 1. Total Credits Requirement - 120 credits required



% total_credits_check(Courses)/1: the check for total number of credits required.
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
	write('You still have '), 
	write(Remainder), 
	write(' number of credits to hit the Total Credits Requirement!\n').
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
    get_comm_credits(Courses, TotalCredit),
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
	write('You still have '), 
	write(Remainder), 
	write(' number of credits remaining to hit the Communication Requirement!\n').
print_communication_check_message(NumCredits) :-
	NumCredits >= 6,
	write('Congrats! You have fulfilled the Communication Requirement!\n').


get_comm_credits([], 0).
get_comm_credits(Courses, Credits) :-
	setof(X, comm_course(X), Bag),
	intersect_length(Courses, Bag, Common),
	Credits is 3 * Common.


%________________________________________________________________________________________________________________________________

% 3. Art Requirement - Total of 12 credits from valid Art subjects. Communication subjects can not be double counted.



% arts_check(Courses)/1: the check for arts requirement.
% Arguments:
%   Courses: The list of courses, each one a string.
%
arts_check(Courses) :-
	write("\n3. Arts Requirements (12 Credits) :\n"),
    get_arts_credits(Courses, TotalCredit),
    write('The total number of arts credits you have is: '),
    write(TotalCredit),
    nl,
    print_arts_check_message(TotalCredit).

print_arts_check_message(NumCredits) :-
	NumCredits < 12,
	Remainder is 12 - NumCredits,
	write('You still have '), 
	write(Remainder), 
	write(' number of credits remaining to hit the Arts Requirement!\n').

print_arts_check_message(NumCredits) :-
	NumCredits >= 12,
	write('Congrats! You have fulfilled the Arts Requirement!\n').

get_arts_credits([], 0).
get_arts_credits(Courses, Credits) :-
	setof(X, arts_subject(X), Bag),
	count_art_subject_in_common(Courses, Bag, Common),
	get_comm_credits(Courses, CommCredits),
	Credits is 3 * Common - CommCredits.

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
	do_check(Courses, Bag).

do_check(_, []) :- !.
do_check(Courses, [Head | Tail]) :-
	check_required_satisfy(Courses, Head),
	do_check(Courses, Tail).

check_required_satisfy(Courses, Requirements) :-
	intersect_length(Courses, Requirements, Count),
	Count > 0,
	write("Congrats! You have satisfied one of "),
	write(Requirements),
	nl.

check_required_satisfy(Courses, Requirements) :-
	intersect_length(Courses, Requirements, Count),
	Count = 0,
	write("You still need to satisfy one of "),
	write(Requirements),
	nl.


%________________________________________________________________________________________________________________________________

% 5. Upper Year CPSC Requirements
% 9 credits CPSC 300+
% 6 credits CPSC 400+


% upper_year_cpsc_check(Courses)/1: the check for upper-year CPSC credits.
% Arguments:
%   Courses: The list of courses, each one a string.
%
upper_year_cpsc_check(Courses) :-
	write("\n5. Upper-Year CPSC Course Requirements:\n"),
	get_300_plus_cpsc_credits(Courses, Credits300Plus),
	get_400_plus_cpsc_credits(Courses, Credits400Plus),
	Remainder is Credits400Plus - 6,
	Addition is max(Remainder, 0),
	New300Credits is Credits300Plus + Addition,
	check_400_credits(Credits400Plus),
	check_300_credits(New300Credits).

get_300_plus_cpsc_credits([], 0).
get_300_plus_cpsc_credits([Head|Tail], Credits) :-
	parse_course(Head, Subject, Number),
	check_300_plus_cpsc(Subject, Number, C),
	get_300_plus_cpsc_credits(Tail, RemainingCredits),
	Credits is C + RemainingCredits.

check_300_plus_cpsc("CPSC", Number, Credits) :-
	atom_number(Number, Atom),
	Atom >= 300,
	Atom < 400,
	Credits is 3.
check_300_plus_cpsc(_, _, 0).

get_400_plus_cpsc_credits([], 0).
get_400_plus_cpsc_credits([Head | Tail], Credits) :-
	parse_course(Head, Subject, Number),
	check_400_plus_cpsc(Subject, Number, C),
	get_400_plus_cpsc_credits(Tail, RemainingCredits),
	Credits is C + RemainingCredits.

check_400_plus_cpsc("CPSC", Number, Credits) :-
	atom_number(Number, Atom),
	Atom >= 400,
	Credits is 3.
check_400_plus_cpsc(_, _, 0).

check_400_credits(Credits) :-
	write("The number of 400 + credits is "),
	C is min(Credits, 6),
	write(C),
	nl.

check_300_credits(Credits) :-
	write("The number of 300 + credits is "),
	write(Credits),
	nl.

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
	write("number of physical credits is "),
	write(PhysCredits),
	nl,
	write("number of biology credits is "),
	write(BiolCredits),
	nl,
	write("number of chemisty credits is "),
	write(ChemCredits),
	nl.


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

%________________________________________________________________________________________________________________________________

% 7. Upper Year Credits 


% Upper Year Requirement
% 48 credits of upper year courses
% 30 credits of upper year science courses





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



