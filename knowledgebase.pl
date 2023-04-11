% course(Course, Credits)/2: courses with special number of credits (not equal to 3)
% Arguments:
%   Course: A string, representing a course
%   Credits: An integer, the number of credits for that course
%
course("CPSC 110", 4).
course("CPSC 121", 4).
course("CPSC 210", 4).
course("CPSC 221", 4).
course("CPSC 213", 4).
course("CPSC 310", 4).
course("CPSC 319", 4).
course("CPSC 310", 4).
course("CPSC 448", 6).

% default_credits(Credits)/1: the default number of credits for a course (== 3)
% Arguments:
%   Credits: The default credit value for a course is 3
%
default_credits(3).


% comm_course(Course)/1: Courses that fulfill the communication requirement
% Arguments:
%   Course: A string, representing a course
%
comm_course("WRDS 150").
comm_course("SCIE 113").
comm_course("ENGL 100").
comm_course("ENGL 110").
comm_course("ENGL 111").
comm_course("ENGL 112").
comm_course("ENGL 120").
comm_course("ENGL 121").


% arts_course(Course)/1: Courses that fulfill the arts requirement
% Arguments:
%   Course: A string, representing a course
%
arts_subject("ACAM").
arts_subject("AFST").
arts_subject("AMNE").
arts_subject("ANTH").
arts_subject("ARBC").
arts_subject("ARBM").
arts_subject("ARCL").
arts_subject("ARST").
arts_subject("ARTH").
arts_subject("ASIA").
arts_subject("ASL").
arts_subject("ASLA").
arts_subject("CCST").
arts_subject("ASTU").
arts_subject("CDST").
arts_subject("CENS").
arts_subject("CHIL").
arts_subject("CHIN").
arts_subject("CLST").
arts_subject("CNTO").
arts_subject("COLX").
arts_subject("CRWR").
arts_subject("CSIS").
arts_subject("CTLN").
arts_subject("DANI").
arts_subject("DMED").
arts_subject("ECON").
arts_subject("ENGL").
arts_subject("ENST").
arts_subject("FIRP").
arts_subject("FIST").
arts_subject("FMST").
arts_subject("FNEL").
arts_subject("FNIS").
arts_subject("FREN").
arts_subject("GEOG").
arts_subject("GEOS").
arts_subject("GERM").
arts_subject("GREK").
arts_subject("GRSJ").
arts_subject("BENR").
arts_subject("HESO").
arts_subject("HINU").
arts_subject("HIST").
arts_subject("IAR").
arts_subject("IEST").
arts_subject("INDO").
arts_subject("FIST").
arts_subject("INLB").
arts_subject("INFO").
arts_subject("ITAL").
arts_subject("ITST").
arts_subject("JAPN").
arts_subject("JRNL").
arts_subject("KORN").
arts_subject("LAIS").
arts_subject("LASO").
arts_subject("LAST").
arts_subject("LATN").
arts_subject("LIBR").
arts_subject("LING").
arts_subject("MDIA").
arts_subject("MDVL").
arts_subject("MES").
arts_subject("MUSC").
arts_subject("NEPL").
arts_subject("NEST").
arts_subject("PERS").
arts_subject("PHIL").
arts_subject("POLI").
arts_subject("POLS").
arts_subject("PORT").
arts_subject("PPGA").
arts_subject("PSYC").
arts_subject("PUNJ").
arts_subject("RELG").
arts_subject("RGLA").
arts_subject("RMST").
arts_subject("RUSS").
arts_subject("SANS").
arts_subject("SCAN").
arts_subject("SEAL").
arts_subject("SLAB").
arts_subject("SOAL").
arts_subject("SPCO").
arts_subject("SOWK").
arts_subject("SPAN").
arts_subject("SWED").
arts_subject("THFL").
arts_subject("THTR").
arts_subject("TIBT").
arts_subject("UKRN").
arts_subject("VISA").
arts_subject("WRDS").


% required(Course)/1: Describes required courses for CPSC major
% Arguments:
%   Course: A lsit of string, each representing a course that can be equally substitutied with each othger
%
required(["CPSC 100"]).
required(["CPSC 121"]).
required(["CPSC 210"]).
required(["CPSC 221"]).
required(["CPSC 213"]).
required(["CPSC 310"]).
required(["CPSC 313"]).
required(["MATH 100", "MATH 102", "MATH 104", "MATH 180", "MATH 184", "MATH 120", "MATH 110"]).
required(["MATH 101", "MATH 103", "MATH 105", "MATH 121"]).
required(["MATH 200", "MATH 226"]).
required(["MATH 221", "MATH 223"]).
required(["STAT 241", "STAT 351", "STAT 200"]).
