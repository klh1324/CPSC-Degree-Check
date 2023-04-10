:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(pcre)).
:- use_module(library(listing)).
:- dynamic course/10.


% Define the URL to download
url('https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-department&dept=CPSC').

% Download the HTML content of the web page
%debug.
download_webpage(Webpage) :-
    url(URL),
    http_open(URL, In, []),
    load_html(In, Webpage, []),
    close(In).

extract_course_titles(Webpage, Titles) :-
    % Parse the HTML content into a Prolog term
    load_structure(stream(Webpage), DOM, []),
    % Extract the `td` tags under the `tr` tags with class `section1` and `section2`
    findall(TD, (member(tr(class=[section1, section2], _, [TD|_]), DOM), nth0(1, TD, Title)), Titles).


% Extract the links to each course webpage
extract_course_links(CourseLinks) :-
    download_webpage(Webpage),% write('COURSES'),
    xpath(Webpage, //tr, Tr),
    xpath(Tr, //td/a(@href), A),% write(A),
    findall(A, (sub_string(A, _, _, _, A)), ListCourse), append([], [ListCourse], CourseLinks).


% Extract the course information from each course webpage
extract_course_info(ListCourseLink, Status, Name, Section, Activity, Term, Mode_of_Delivery, Days, ST, ET, In_Person_Required) :-
    atomics_to_string(ListCourseLink, CourseLink),
    atom_concat('https://courses.students.ubc.ca', CourseLink, PROTO_URL),
    % Remove "&amp;"
    re_replace("&amp;"/g, "&", PROTO_URL, URL),
    http_open(URL, In, []),
    load_html(In, Webpage, []),
    close(In),
    xpath(Webpage, //tr, TR),
    % course Status
    xpath(TR, //td(1), PreTD1), xpath(PreTD1, /self(normalize_space), TD1),
    findall(TD1, (sub_string(TD1, _, _, _, TD1)), Status), 
    % course name and section
    xpath(TR, //td/a(normalize_space), A), %write(Tds),
    findall(A, (sub_string(A, _, _, _, A)), ListCourse), atomics_to_string(ListCourse, Course), sub_string(Course, _, 3, 0, Section), 
    sub_string(Course, 0, 8, _, Name),
    % course activity
    xpath(TR, //td(3), PreTD3), xpath(PreTD3, /self(normalize_space), TD3),
    findall(TD3, (sub_string(TD3, _, _, _, TD3)), Activity), 
    %course term
    xpath(TR, //td(4), PreTD4), xpath(PreTD4, /self(normalize_space), TD4),
    findall(TD4, (sub_string(TD4, _, _, _, TD4)), Term),
    %course mode of delivery
    xpath(TR, //td(5), PreTD5), xpath(PreTD5, /self(normalize_space), TD5),
    findall(TD5, (sub_string(TD5, _, _, _, TD5)), Mode_of_Delivery), 
    %course days
    xpath(TR, //td(7), PreTD7), xpath(PreTD7, /self(normalize_space), TD7),
    findall(TD7, (sub_string(TD7, _, _, _, TD7)), Days), 
    %course start time
    xpath(TR, //td(8), PreTD8), xpath(PreTD8, /self(normalize_space), TD8),
    findall(TD8, (sub_string(TD8, _, _, _, TD8)), ST), 
    %course end time
    xpath(TR, //td(9), PreTD9), xpath(PreTD9, /self(normalize_space), TD9),
    findall(TD9, (sub_string(TD9, _, _, _, TD9)), ET), 
    %course in person required
    xpath(TR, //td(11), PreTD11), xpath(PreTD11, /self(normalize_space), TD11),
    findall(TD11, (sub_string(TD11, _, _, _, TD11)), In_Person_Required).
    %create course predicate
    %assertz(course(Status, Name, Section, Activity, Term, Mode_of_Delivery, Days, ST, ET, In_Person_Required)).


% Create a list of course predicates from the course webpages
create_course_list(CourseList) :-
    extract_course_links(CourseLinks),
    findall(CourseLinks,
    extract_course_info(CourseLinks, Status, Name, Section, Activity, Term, Mode_of_Delivery, Days, ST, ET, In_Person_Required), CourseList).

process_course_links :-
    extract_course_links(CourseLinks),
    process_course_links_helper(CourseLinks).

process_course_links_helper([]).
process_course_links_helper([CourseLink|Rest]) :-
    extract_course_info(CourseLink, Status, Name, Section, Activity, Term, Mode_of_Delivery, Days, ST, ET, In_Person_Required),
    write('course('), write(Status), write(','), write(Name), write(','), write(Section), write(','),  write(Activity), 
    write(','), write(Term), write(','), write(Mode_of_Delivery), write(','), write(Days), write(','), write(ST),
    write(ET), write(In_Person_Required), write(').\n'),
    %assert(course(Status, Name, Section, Activity, Term, Mode_of_Delivery, Days, ST, ET, In_Person_Required)),
    process_course_links_helper(Rest).

create_file :- extract_course_links(Courses), maplist(assert, Courses).
main :-
    tell('database.pl'),
    (
        process_course_links,
        %listing(course/10),
        fail
    ;
        told
    ).
