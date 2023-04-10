:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- dynamic course/9.


% Define the URL to download
url('https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-department&dept=CPSC').

% Download the HTML content of the web page
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
    download_webpage(Webpage),
    xpath(Webpage, //(a(@href)), Links),
    findall(Links, (sub_string(Links, _, _, _, 'course=')), CourseLinks).

% Extract the course information from each course webpage
extract_course_info(CourseLink, Course) :-
    atom_concat('https://courses.students.ubc.ca', CourseLink, PROTO_URL),
    % Remove "&amp;"
    re_replace("&amp;"/g, "&", PROTO_URL, URL),
    http_open(URL, In, []),
    load_html(In, Webpage, []),
    close(In),
    xpath(Webpage, //(td(normalize_space)), Tds), write(Tds),
    maplist(atom_codes, TdCodes, Tds),
    [StatusCodes, CourseCodes, TitleCodes, SectionCodes, ActivityCodes, TermCodes, DeliveryCodes, DaysCodes, StartTimeCodes, EndTimeCodes] = TdCodes,
    dif(CourseCodes, ''),
    dif(SectionCodes, ''),
    (TitleCodes = [] ->
        Title = 'Unknown';
        atom_codes(Title, TitleCodes)),
    atom_codes(CourseCodeAll, CourseCodes),
    split_string(CourseCodeAll, ' ', '', CourseCodeWords),
    last(CourseCodeWords, SectionCode),
    CourseCodeWords = [_, CourseNameCodes | _],
    exclude(empty_string, CourseNameCodes, CourseNameCodesNoSpaces),
    maplist(atom_codes, CourseNameWords, CourseNameCodesNoSpaces),
    CourseCode =.. [course | [StatusCodes, CourseCodeAll | CourseNameWords]],
    Course =.. [course, StatusCodes, CourseCode, Title, SectionCode, ActivityCodes, TermCodes, DeliveryCodes, DaysCodes, StartTimeCodes, EndTimeCodes].

% Helper predicate to exclude empty strings
empty_string("").

% Create a list of course predicates from the course webpages
create_course_list(CourseList) :-
    extract_course_links(CourseLinks),
    findall(Course, (member(CourseLink, CourseLinks), extract_course_info(CourseLink, Course)), CourseList).

:- create_course_list(Courses), maplist(assert, Courses).
write_courses_to_file :- tell('knowledgebase.pl'),
   listing(course/9),
   told.
