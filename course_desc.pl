:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(apply)).
:- use_module(library(pcre)).
:- use_module(library(listing)).

% Define the URL to download
url('https://ubccsss.org/services/courses/').

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
    load_html(stream(In), Webpage, []),
    close(In),
    %xpath(Webpage, //dl/dd(@class="col-sm-9"), Title),write(23).
    xpath(Webpage, //(header)/h1(@class='blog-post-title'), CourseCode), xpath(CourseCode, /self(normalize_space), Cc),
    xpath(Webpage, //(header)/h3(@class='blog-post-title mb-0'), Title), xpath(Title, /self(normalize_space), Ti),
    xpath(Webpage, //(div)/p(@class='card-text'), Description), xpath(Description, /self(normalize_space), Desc),
    atomic_list_concat([Cc, ' - ', Ti, ' : ', 'Course Description - ', Desc], CourseInfo).


get_info(Number, Description) :-
    url(URL), atom_concat(URL, 'cpsc-', Pre_Course_url), atom_concat(Pre_Course_url, Number, Course_url),
    extract_course_info(Course_url, Description).