:- include('requirement_check.pl').

% start: The entry point to the recommendation program 
start :-
    write('Hi there! Thanks for using this reccomendation system!\n'),
    write('Enter the filename where your taken courses are. It should be enrapped in quotes (e.g. \'courses.txt\'.) \n'),
    read(Input),
    write('\nYou have taken the following courses:\n'),
    read_lines(Input, Courses),
    print_lines(Courses),
    total_credits_check(Courses),
    communication_check(Courses),
    arts_check(Courses),
    required_check(Courses),
    upper_year_cpsc_check(Courses),
    science_breadth_check(Courses),
    upper_year_credit_check(Courses),
    write('Bye!\n').


% read_lines(File, Lines)/2: reads all the lines from a file. If the provided filename is invalid tries again and prompts the user to enter a valid filename.
% Arguments:
%   File: A string, the name of the file to be read 
%   Lines: A list of strings, each representing a line in the file
%
read_lines(File, Lines) :-
    exists_file(File),
    read_file(File, Content),
    split_string(Content, "\n", "", Lines).

read_lines(File, Lines) :-
    \+ exists_file(File),    
    write('That file does not exist. Try again.\n'),
    write('Enter the filename where your taken courses are. It should be enrapped in quotes (e.g. \'courses.txt\'.) \n'),
    read(Input),
    read_lines(Input, Lines).


% read_file(File, Lines)/2: opens the file, read the contents, and closes the file.
% Arguments:
%   File: A string, the name of the file to be opened and closed.
%   Content: A string, representing the entire content of the file.
%
read_file(File, Content) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    atom_codes(Content, Codes).


% print_lines(Lines)/1: lines is printed to standard output, with each string seperated by a new line.
% Arguments:
%   Lines: A list of string to be printed out.
%
print_lines([]).
print_lines([Line|Rest]) :-
    write(Line), 
    nl,
    print_lines(Rest).


% parse_courses(Courses)/1: Parses entire list of courses and checks graduation requirements.
% Arguments:
%   Courses: A list of courses read from the text file.
%
parse_courses([]).
parse_courses([Course|Rest]) :-
    parse_course(Course, Subject, Code),
    write('the subject number is '),
    write(Subject),
    write(' and the code number is '),
    write(Code),
    nl,
    parse_courses(Rest).

