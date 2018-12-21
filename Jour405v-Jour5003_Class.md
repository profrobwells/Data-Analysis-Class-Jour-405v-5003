Jour 405v-Jour 5003 Data Analysis
================
Wells
12/21/2018

Inside Twitter: Data Analysis for Journalists
=============================================

Jour 405v, Jour 5003, Spring 2019
---------------------------------

### Rob Wells, Ph.D.

#### University of Arkansas

#### School of Journalism and Strategic Media

> <rswells@uark.edu>

> @rwells1961

**Course Goal:** Students will learn the latest data journalism techniques that drive modern newsrooms and public relations / advertising offices. The class will extract and analyze Twitter data with the goal of producing an interactive multimedia presentation.

**Course Description:** This course will teach students how to code in programs such as R and SQL and how these powerful tools are used in modern news reporting. Quality reporting in newsrooms requires a solid foundation of data analysis. The data skills taught in this class are in high demand in newsrooms and corporations. &gt;

![You will learn to love this program](Images/RStudio-Logo-Blue-Gradient.png)

**Required Text:** Machlis, Sharon. Practical R for Mass Communications and Journalism. Chapman & Hall/CRC The R Series. 2018. ISBN 9781138726918 <https://www.amazon.com/gp/search?keywords=9781138726918>

> <a href="http://www.machlis.com/R4Journalists/" target="_blank">Link to several free chapters: &quot;CNTL&quot; + click for a New Tab</a>

> <a href="https://docs.google.com/document/d/1O4o5V_GomJPa6ojLeE48y9LhKhmbzTsWu9jskcZKkbY/edit" target="_blank"><strong>Syllabus - Jour 405v:</strong>: &quot;CNTL&quot; + click for a New Tab</a>

> <a href="https://docs.google.com/document/d/19khrHXSqHIFfRwLRlI_mD3cVNv2gHqj9Ze1GDs22Bis/edit" target="_blank"><strong>Syllabus - Jour 5003</strong>: &quot;CNTL&quot; + click for a New Tab</a>

> <a href="https://docs.google.com/spreadsheets/d/1onfY79xjhQVetnFbNvw1z3zLGrSQ1xkAoo0qoCRElGM/edit#gid=681742372" target="_blank"><strong>Course Schedule</strong>: &quot;CNTL&quot; + click for a New Tab</a>

Week \#1: Introduction and R Basics
-----------------------------------

**Agenda:** --Email to students.

--Intro R and R Studio. Open program. <https://docs.google.com/presentation/d/1zICxR7qDM3RQ2Nxi5CqHlM3H8I7qoVkNtqcNcnbbDCw/edit#slide=id.p>

--Load tutorial &gt; <a href="Introduction-to-R-Oct-29-2018-1c3su5h.r" target="_blank"><strong>Download this file and open it in R Studio</strong>: &quot;CNTL&quot; + click for a New Tab</a>

--R interface explained. <https://docs.google.com/presentation/d/1O0eFLypJLP-PAC63Ghq2QURAnhFo6Dxc7nGt4y_l90s/edit#slide=id.g1bc441664e_0_10>

--Show basic R skills.

--Loading software.

--Workflow.

--Conventions in coding.

--Practice with dataset.

**Reading:**

--Machlis. Chapter 1 & 2.

--Beginner's guide to R: <https://www.computerworld.com/article/2497143/business-intelligence/business-intelligence-beginner-s-guide-to-r-introduction.html>

\*\*<Notes:**> --Data Wrangling-Text Mining in Twitter. --See entire scraping sequence. Extract from Twitter. --Basic descriptive statistics --Chart --Export Static --Story

**Resources:** RStudio IDE Easy Tricks You Might’ve Missed <https://rviews.rstudio.com/2016/11/11/easy-tricks-you-mightve-missed/>

How Do I? <https://smach.github.io/R4JournalismBook/HowDoI.html>

Functions <https://smach.github.io/R4JournalismBook/functions.html>

Packages <https://smach.github.io/R4JournalismBook/packages.html>

Week \#2: Use R instead of Excel. R Markdown. Loading and basic file management
-------------------------------------------------------------------------------

**Agenda:** --Use R instead of Excel

--R Markdown --Loading and basic file management

**Reading:**

--Machlis. Chapter 3 & 4.

--Andrew Ba Tran

Week \#3: R: Scripts, Workbooks, Markdown. GitHub
-------------------------------------------------

GitHub
======

This class is intended to teach you modern workflow techniques for coding. A centerpiece of that workflow is GitHub. This is a website with a system that allows you to collaborate with other programmers on coding projects. It manages versions of software code and is a very popular with the tech elite.

Your GitHub account, which is public, represents an important professional image. Prospective employers and collaborators will look at your GitHub account.

--Create a GitHub account. <https://github.com/>

--Follow this tutorial

<https://guides.github.com/activities/hello-world/>

--Terminology
*Commit
*Branch
*Pull Request
*Fork

--Exercise
*Pair up.
*Team 1 takes this code. Make XXX changes.
*Team 2 forks the code. Makes XXX changes.
*Pull & Commit

**Resources on GitHub**

--GitHub flow
<https://guides.github.com/introduction/flow/>

--GitHub Guides
<https://guides.github.com/>

Week \#5: Using R to build basic graphs and charts
--------------------------------------------------

**Agenda** --Rendering html as an output in GitHub
<https://rmarkdown.rstudio.com/lesson-9.html>

**Reading:**
Machlis Chs. 9 & 10
Albert Cairo, "The Functional Art," Principles of Data Visualization.

Download R and RStudio
----------------------

This program is loaded on all of the computers in Kimpel 146. You can load it on your laptop as well. It is not large and doesn't tax the memory a lot. R runs on Windows, Mac and Linux, but this course uses the Mac version. If you use Windows on your personal laptop, the instructor is not responsible for any variations in the lessons and instructions.

First, download the most recent version of R at <https://www.r-project.org/>, . Look for the link to download R. Click that download option and you should be taken to CRAN, the Comprehensive R Archive Network, and a list of CRAN servers, called mirrors, around the world. Pick a server and choose the precompiled binary distribution for your operating system. Once the file finishes downloading, install it like any other software program – run the .exe for Windows or .pkg for Mac.

Accept all of the default settings for Mac.

Second, install RStudio, an excellent user interface that helps you manage and create R code. Download the open source edition of R Studio desktop and follow the prompts to install it. <http://www.rstudio.com/products/rstudio/download/>

More information: <http://www.machlis.com/R4Journalists/download-r-and-rstudio.html>

R Markdown
----------

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

Including Plots
---------------

You can also embed plots, for example:

![](Jour405v-Jour5003_Class_files/figure-markdown_github/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.