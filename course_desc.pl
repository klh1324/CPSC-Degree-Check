:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(pcre)).
:- use_module(library(listing)).

% Define the URL to download
url('https://ubccsss.org/services/courses/'). % #UBCV').%-2021W-CPSC-100-101.
url2('https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-department&dept=CPSC').

% Download the HTML content of the web page
%debug.
download_webpage(Webpage) :-
    url(URL),
    http_open(URL, In, []),
    load_html(In, Webpage, []),
    close(In).


extract_course_info(URL, CourseInfo) :- 
    catch(http_open(URL, In, []), Error, (
    % Handle the error
    format('Invalid input, please try again.', [])
)),
    %http_open(URL, In, []),
    load_html(stream(In), Webpage, []),
    close(In),
    %xpath(Webpage, //dl/dd(@class="col-sm-9"), Title),write(23).
    xpath(Webpage, //(header)/h1(@class='blog-post-title'), CourseCode), xpath(CourseCode, /self(normalize_space), Cc),
    xpath(Webpage, //(header)/h3(@class='blog-post-title mb-0'), Title), xpath(Title, /self(normalize_space), Ti),
    xpath(Webpage, //(div)/p(@class='card-text'), Description), xpath(Description, /self(normalize_space), Desc),
    atomic_list_concat([Cc, ' - ', Ti, ' - ', Desc], Courseinfo), write(Courseinfo).
    %atomics_to_string(Cc,CCC), atomics_to_string(Ti, TTT), atomics_to_string(Desc, Dess), 
    %atom_concat(Cc, Ti, acc1), write(acc1).
    % xpath(Webpage, //h2(@class=mb-0), Title),
    /*xpath(Webpage, //div(@id='vg-headmatter-v2'), a_to_l), write(23),
    xpath(a_to_l, /div(1)/h2(text), average),
    xpath(a_to_l, //div(2)/h2(text), stdev),
    xpath(a_to_l, //div(2)/h2(text), high),
    xpath(a_to_l, //div(3)/h2(text), low),
    xpath(Webpage, //div(@id='teaching-team-v2'), PreteachingTeam), 
    xpath(PreteachingTeam, /self(normalize_space), teachingTeam), write(teachingTeam).*/

get_info(Number, Description) :-
    url(URL), atom_concat(URL, 'cpsc-', Pre_Course_url), atom_concat(Pre_Course_url, Number, Course_url),
    extract_course_info(Course_url, Description).
