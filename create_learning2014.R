Elsa Marques, November 8th 2017 - Regression chapter RStudio exercise data

"\t" 
Variable descriptions and other meta data of JYTOPKYS3 (syksy 2016)

Kimmo Vehkalahti: ASSIST 2014 - Phase 3 (end of Part 2), N=183
Course: Johdatus yhteiskuntatilastotieteeseen, syksy 2014
(Introduction to Social Statistics, fall 2014 - in Finnish),
international survey of Approaches to Learning, made possible
by Teachers' Academy funding for KV in 2013-2015.

Data collected: 3.12.2014 - 10.1.2015/KV
Data created: 14.1.2015/KV, in English 9.4.2015/KV,Florence,Italy
Imputation 4.4.2015: only missing information in certain backgrounds,
minimal amount of missing values imputed using Phases 1 and 2.

Variables names and short descriptions:

Aa       Making sure you remember things well
Ab       Developing as a person
Ac       Building up knowledge by acquiring facts and information
Ad       Being able to use the information you've acquired
Ae       Understanding new material for yourself
Af       Seeing things in a different and more meaningful way
ST01     I manage to find conditions for studying which allow me to get on with my work easily.
SU02     Often I find myself wondering whether the work I'm doing here is really worthwhile.
D03      I usually set out to understand for myself the meaning of what we have to learn.
ST04     I organize my study time carefully to make the best use of it.
SU05     I find I have to concentrate on just memorising a good deal of what I have to learn.
D06      I look at the evidence carefully and try to reach my own conclusion about what I'm studying.
D07      I try to relate ideas I come across to those in other topics or other course whenever possible.
SU08     I tend to read very little beyond what is actually required to pass.
ST09     I think I'm quite systematic and organised when it comes to revising for exams.
SU10     There's not much of the work here that I find interesting or relevant.
D11      When I'm reading an article or book, I try to find out for myself exactly what the author means.
ST12     I'm pretty good at getting down to work whenever I need to.
SU13     Much of what I'm studying makes little sense: it's like unrelated bit and pieces.
D14      When I'm working on a new topic, I try to see in my own mind how all the ideas fit together.
D15      Often I find myself questioning things I hear in lectures or read in books.
SU16     I concentrate to learning just those bits of information I have to know to pass.
ST17     I'm good at following up some of the reading suggested by lecturers or tutors.
SU18     When I look back, I sometimes wonder why I ever decided to come here.
D19      When I'm reading I stop from time to time to reflect on what I am trying to learn from it.
ST20     I work steadily through the term or semester, rather than leave it all until the last minute.
SU21     I'm not really sure what's important in lectures, so I try to get down all I can.
D22      Ideas in course books or articles often set me off on long chains of thought of my own.
D23      When I read, I examine the details carefully  to see how they fit in with what's being said.
SU24     I gear my studying closely to just what seems to be required for assignments and exams.
ST25     I usually plan out my week's work in advance, either on paper or in my head.
SU26     I'm not really interested in this course, but I have to take it for other reasons.
D27      Before tackling a problem or assignment, I first try to work out what lies behind it.
ST28     I generally make good use of my time during the day.
SU29     I often have trouble in making sense of the things I have to remember.
D30      I like to play around with ideas of my own even if they don't get me very far.
D31      It's important for me to be able to follow the argument, or to see the reason behind things.
SU32     I like to be told precisely what to do in essays or other assignments.
Ca       Lecturers who tell us exactly what to put down in our notes.
Cb       Lecturers who encourage us to think for ourselves and show us how they themselves think.
Cc       Exams which hallow me to show that I've thought about the course material for myself.
Cd       Exams or tests which need only the material provided in our lecture notes.
Ce       Courses in which it's made very clear just which books we have to read.
Cf       Courses where we're encouraged to read around the subject a lot for ourselves.
Cg       Books which challenge you and provide explanations which go beyond the lectures.
Ch       Book which give you definite facts and information which can easily be learned.
Da       I feel confident I can master statistics topics
Db       Statistics will be useful for my future work
Dc       I am interested in understanding statistics
Dd       I did well in high school mathematics courses
De       I am interested in learning statistics
Df       I feel insecure when I have to do statistics problems
Dg       I like statistics
Dh       I am scared by statistics
Di       I feel confident in doing math operations
Dj       Statistics will be useful for my future studies
Age      Age (in years) derived from the date of birth
Attitude Global attitude toward statistics
Points   Exam points
gender   Gender: M (Male), F (Female)


Background of measures:

Measures A and C are based on parts A and C in ASSIST
(Approaches and Study Skills Inventory for Students)
http://www.etl.tla.ed.ac.uk/publications.html#measurement
and D is based on SATS (Survey of Attitudes Toward Statistics)
http://www.evaluationandstatistics.com/

The items of the central measure in this study (ASSIST B)
are named so that the connections to the corresponding
dimensions (Deep/SUrface/STrategic) can be easily seen.
Compared to the original 52 item ASSIST we here used a brief
version (created by Primi, Chiesi et al), including 32 items.

The survey was done originally in Finnish. The measures A,C,D
were translated in Finnish by Kimmo Vehkalahti and Liisa Myyry.
Measure B had been earlier translated by Miia Erkkilä (2008), see
http://lib.tkk.fi/Raportit/2009/isbn9789512297719.pdf

Some background and other variables are omitted from this version,
otherwise all 183 responses are included. There are no missing values.

Data are used for teaching purposes from 1/2017 on with a course
Introduction to Open Data Science.

Some preliminary results were shown in Rio de Janeiro (ISI 2015):
http://www.slideshare.net/kimmovehkalahti/the-relationship-between-learning-approaches-and-students-achievements-in-an-introductory-statistics-course-in-finland

--------------------------------------------------------------

Information about creating further variables are found in:
http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt





#in the database you can find the results obtained from the students assisting the course in 2014 course

# total of 183 responses included


```{r learning2014, echo = FALSE}
#analyse "learning2014" data
str(learning2014)
summary(learning2014)
```

