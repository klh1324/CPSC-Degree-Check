:- include('requirement_check.pl').

:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(pcre)).
:- use_module(library(listing)).

% Start program with:
% [recommender].
% start.


% start: The entry point to the recommendation program 
start :-
    write('Hi there! Thanks for using this recommendation system!\n'),
    write('Enter the filename where your courses are. It should be enrapped in quotes (e.g. \'courses.txt\'.) \n'),
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
    nl,
    write('**************************************************************************\n'),
    display_cpsc_course_info.


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


%________________________________________________________________________________________________________________________________

% CPSC Course Description Scraper 

% Base URL to scrape from
url('https://ubccsss.org/services/courses/').

% display_cpsc_course_info/0: Recursive predicate that allows users to find CPSC course descriptions.
%
display_cpsc_course_info :-
    nl,
    write("If you are interested in knowing more about CPSC courses, enter a course number (e.g. 221). To quit, type q\n"),
    read(Input),
    handle_input(Input).


% handle_input(Input)/1: Handles user input. Decides whether to quit program or search for CPSC course description.
% Arguments:
%   Input: The user input. If q, "q", quit, or "quit" the program terminates. Otherwise input should be CPSC course number (e.g. 221).
handle_input(q) :- 
    write("Quitting!\n"), halt.
handle_input("q") :- 
    write("Quitting!\n"), halt.
handle_input(quit) :-
    write("Quitting!\n"), halt.
handle_input("quit") :-
    write("Quitting!\n"), halt.
handle_input(Number) :- 
    get_info(Number),
    display_cpsc_course_info.



% get_info(Number)/1: Given a CPSC course number, tries to scrape description from the ubccss website.
% Arguments:
%   Number: A valid CPSC course number. 
% 
get_info(Number) :-
    url(URL),
    atom_concat(URL, 'cpsc-', Pre_Course_url),
    atom_concat(Pre_Course_url, Number, Course_url),
    extract_course_info(Course_url).


% extract_course_info(Url)/1: Given a URL, tried to scrape the CPSC course description from it
% Arguments:
%   URL: The CPSC course URL for ubccsss 
%
extract_course_info(URL) :- 
    catch(http_open(URL, In, []), _, (
        handle_error
    )),
    load_html(stream(In), Webpage, []),
    close(In),
    xpath(Webpage, //(header)/h1(@class='blog-post-title'), CourseCode), 
    xpath(CourseCode, /self(normalize_space), Cc),
    xpath(Webpage, //(header)/h3(@class='blog-post-title mb-0'), Title),
    xpath(Title, /self(normalize_space), Ti),
    xpath(Webpage, //(div)/p(@class='card-text'), Description),
    xpath(Description, /self(normalize_space), Desc),
    atomic_list_concat([Cc, ' - ', Ti, ' : ', 'Course Description - ', Desc], CourseInfo),
    write(CourseInfo),
    nl.


% handle_error/0: Given a http_load error, loops back to original prompt asking user for valid CPSC course number.
% 
handle_error :-
    write("Sorry, that was not a valid CPSC course number"),
    display_cpsc_course_info.
