:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(pcre)).
:- use_module(library(listing)).

% Define the URL to download
url('https://ubcgrades.com'). % #UBCV').%-2021W-CPSC-100-101.
url2('https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-department&dept=CPSC').

% Download the HTML content of the web page
%debug.
download_webpage(Webpage) :-
    url(URL),
    http_open(URL, In, []),
    load_html(In, Webpage, []),
    close(In).
download_CourseNumPage(CourseNumPage) :-
    url2(URL),
    http_open(URL, In, []),
    load_html(In, CourseNumPage, []),
    close(In).
% Extract the links to each course webpage
extract_course_links(CourseNum, Section) :-
    download_webpage(Webpage), write('COURSES'),
   % download_CourseNumPage(CourseNumPage),
   % xpath(CourseNumPage, //td/a(normalize_space), CourseNum), write(Course), sub_string(CourseNum, _, 3, 0, CourseNum),
    %write(CourseNum),
    xpath(Webpage, //select(@id=vg-drop-course)/option(@value=CourseValue), CourseNum),
    xpath(Webpage, //select(@id=vg-drop-section)/option(@value=SecValue), Section), write(235).
    %xpath(Webpage, //(a(@href)), PreLinks),
    %xpath(PreLinks, /self(normalize_space), Links), write(Links),
    %xpath(Webpage, //tr, Tr),
    % course name
    %xpath(Tr, //td/a(@href), A),% write(A),
    %xpath(PreA, /self(normalize_space), A), %write(5354787),
    %findall(A, (sub_string(A, _, _, _, A)), ListCourse), append([], [ListCourse], CourseLinks).

extract_course_info(URL, Title, average, stdev, high, low, teachingTeam) :- 
    http_open(URL, In, []),
    load_html(In, Webpage, []),
    close(In),
    xpath(Webpage, //div(@id='tableau-dashboard-row')/div/div/div/div/div/h2(text), Title),write(Title),
    % xpath(Webpage, //h2(@class=mb-0), Title),
    xpath(Webpage, //div(@id='vg-headmatter-v2'), a_to_l), write(23),
    xpath(a_to_l, /div(1)/h2(text), average),
    xpath(a_to_l, //div(2)/h2(text), stdev),
    xpath(a_to_l, //div(2)/h2(text), high),
    xpath(a_to_l, //div(3)/h2(text), low),
    xpath(Webpage, //div(@id='teaching-team-v2'), PreteachingTeam), 
    xpath(PreteachingTeam, /self(normalize_space), teachingTeam), write(teachingTeam).


main :-
    extract_course_info('https://ubcgrades.com', _, _, _, _, _, _),
    extract_course_info('https://ubcgrades.com/#UBCV-2021W-CPSC-213-101', _, _, _, _, _, _).
    /*tell('database.pl'),
    (
        process_course_links,
        %listing(course/10),
        fail
    ;
        told
    ).*/
