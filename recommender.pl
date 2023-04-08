% start: The entry point to the recommendation program 
start :-
    write('Hi there! Thanks for using this reccomendation system!\n'),
    write('Enter the filename where your taken courses are. It should be enrapped in quotes (e.g. \'courses.txt\'.) \n'),
    read(Input),
    read_lines(Input, Lines),
    print_lines(Lines),
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


% print_lines(lines)/1: lines is printed to standard output, with each string seperated by a new line.
% Arguments:
%   Lines: A list of string to be printed out.
%
print_lines([]).
print_lines([Line|Rest]) :-
    write(Line), 
    nl,
    print_lines(Rest).

