[People]
;Name=Function,Function details
Arnaud Bouchez=Main Contributor,Develop software and manage associated projects

[Project]
Name=mORMot2
Company=Synopse
ReleaseVersion=
ReleaseDate=
Manager=Arnaud Bouchez
MainSection=DI
; so we first check for [DI] and [DILayout]
NoRiskDisplay=Not implemented
DestinationDir=D:\Documents\SynProject
; path to store all created .doc (not to be inside versioning tree)
OldWordOpen=No
; if OldWordOpen=Yes, Conversion is made visible on the screen (compatible with some Word 2000 installations)
DefLang=1033
Logo=logo.png
; this picture will be displayed top of every document front page
HeaderColWidth=22,37,22,19
; page header columns width, according to Manager=The Manager's name
NoConfidential=Yes
; so that no "Confidential" text will appear in page footer - seems convenient for a GPL document ;)
HeaderWithLogo=Yes
; custom page header with the synopse logo
HtmlSideBar=Overview/Meet the mORMot:SOURCE,Download/How to install:TITL_113,API Reference/Units and classes:SIDE_MORMOT2,FAQ/Frequently Asked Questions:TITL_123,Forum/Get support:https://synopse.info/forum/viewforum.php?id=24,Open Source/GitHub Official Repository:https://github.com/synopse/mORMot2,Blog/Latest News:http://blog.synopse.info,Donate/Adopt a mORMot!:https://synopse.info/fossil/wiki?name=HelpDonate,Licence Terms/Either MPL, LGPL or GPL:https://github.com/synopse/mORMot2/blob/master/LICENCE.md
; the sidebar first links, for html export

{\b Document License}
{\i Synopse mORMot 2 Framework Documentation}.\line Copyright (C) 2023 Arnaud Bouchez.\line Synopse Informatique - @https://synopse.info
The {\i Synopse mORMot 2 Framework Source Code} is licensed under GPL / LGPL / MPL licensing terms, free to be included in any application - see @https://github.com/synopse/mORMot2/blob/master/LICENCE.md
The {\i Synopse mORMot 2 Framework Documentation} is a free document, released under a GPL 3.0 License.
{\b Trademark Notice}
Rather than indicating every occurrence of a trademarked name as such, this document uses the names only in an editorial fashion and to the benefit of the trademark owner with no intention of infringement of the trademark.

[Pictures]
happymormot.png=300x225 35%,Meet the mORMot
logo.png=200x47 25%,Synopse Logo
IamLost.png=300x283 30%,Map the mORMot
; here are stored the Pictures properties, as PictureFileName=WIDTHxHEIGHT PERCENT%,Caption

[DI]
Owner=DI
Order=DI
Name=Design Input Product Specifications
ItemName=DI
DisplayName=Design Input
Purpose=Create high level description of software specifications
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
; Revision* multiple revision Table: ignored values are taken from current, older below
RevisionDescription=Current Version
RevisionDate=
Revision=2.0
; [DILayout] list the global DI outline (lines beginning with : are titles)
; [DI-*] details all items
DefaultPreparedBy=Arnaud Bouchez
; all [DI-*] PreparedBy= default name
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
;SubDocFrontPage=Warning,PeopleDetails

:System Specifications
; special lines begin with
;   :  for titles (no Name=Value pairs after a :title)
;   :# Title   for a later reference as @#@ (":1 Title" then @1@)
;   -  for a list item
;   !  for pascal source
;   !! for modified pascal source line
;   &  for c c++ source
;   &! for modified c c++ source line
;   #  for c# source
;   #! for modified c# source line
;   $  for text file (fixed-width font)
;   $! for modified text file line (fixed-width font)
;   %filename.jpg [640x480 85%] for images jpg/bmp/png/emf - see [Pictures]
;   %%FirmwareBoot for diagram images, i.e. not listed in [Pictures] but created with \graph FirmwareBoot ...
;   |%30%40%30   then |Col 1|Col 2|Col 3  for every row, ending with |%  for columns
;   |%=-30%40%30  -> =:no indent -:no border
;   =[SectionName] to inline the [SectionName] content at this place
; text can be formated as rtf (with \b \i { } e.g.) - each new text paragraph will be ended with \par
; {} can be used for a \par alone (void lines are just ignored)
; you can link to another item with @SectionName@ (@DI-4.1@ e.g) or @DocName@ (@SRS@) or @PeopleName@ (@A.Bouchez@) or either @%Picture.png@
; you can embedd a picture within a table cell e.g., by using @=%picture.png@ - in this case, this is not a "button"
; internet links will be handled as hyperlink, with @http://synopse.info
; in the [SDD-*] sections, specify @Module\filename.pas@ for each file name, @!Module\filename.pas@ for filename modified or @!procedurename!Module\filename.pas@ in order to specify the procedure name. The corresponding units (and procedures) will be highlited in the [SAD] document. Just click on the button to use the Object Browser window.
; some special lines commands can be entered:
;  \page        to force a new page
;  \landscape   to change the page orientation to landscape
;  \portrait    to change the page orientation to portrait
;  \footer blabla  to change the footer text
;  \Layout      to add a list with all DILayout titles
;  \LayoutPage  idem + associated pages in the document
;  \risk        to add the Risk Assessment Scale table
;  \Source      (for [SAD] section) to add the list of the Source=.. modules
;  \SourcePage  idem + associated pages in the document
;  \include filename.ext    ext will be used to append !&#$ left
;  \graph UniqueImageName [Title] then following lines either .dot normal text, or "\From Text\To Text[\Label between both]" - use F12 to have the dialog
;  \TableSoftwareChanges or \TableTraceabilityMatrix for SCRS
;  \TableNewFeatures or \TableBugFixes or \TableTests[=October 16, 2008] for Release Notes
;  \TableDI=6.3.1.2,6.3.1.3 for a table with all the supplied Design Inputs
;  \TableDocuments[=DI,SRS,SDD,SAD] for a table with the supplied document details
;  \Implements TableImplementsName #.# [Description][\DocumentName] (\Implements ISO 4.3 Software safety classification) in the text - points to the current document, or the specified DocumentName
;  \TableImplements=TableImplementsName (\TableImplements=ISO) to create the list, sorted by ascending #.# numbers, with description if any and corresponding document
;  =[SectionName] to include this section content at the current place
; in the [Test-*] sections, special auto-defined columns can be used with |Actions[|Expected Results] - manual tables can be used as usual (with |%..)
This document is intended to describe the Design Input Product Specifications.
: Definitions
{\b Added Value} - This level of achievement should be the target of the design team, because achieving this level of performance adds value to the product. However failure to achieve this level does not evoke additional management review.
{\b Must Have} - This level of achievement must be reached in the final design output. Because of possible negative financial impacts, if this level of performance is not achieved, management review will be triggered.
: Project Concept
:  Purpose and Scope
This document focuses on the {\i mORMot 2} library.
The purpose of this @DI@ is to detail the marketing requirements/product specifications for the 2.0 release of the {\i Synopse mORMot Framework library}. The requirements and specifications found in this document are derived from customer market research, regulatory input and industry common practice.
:  Concept Statement
It was observed that a true JSON and RESTful oriented Client-Server framework was missing in the {\i Delphi} programing environment.
Latest versions of {\i Delphi} (i.e. {\i Delphi} 2010 and up) provide a JSON and RESTful mechanism named DataSnap (in the {\i Architect} or {\i Enterprise} editions), but such a feature could be implemented with previous versions of the {\i Delphi} compiler as well, with a more open architecture.
This framework shall use a innovative ORM (Object-relational mapping) approach, based on the RTTI (Runtime Type Information) provided by the {\i Delphi} language. It shall expose Server data access and business services to Clients, using JSON over several communication protocols.
After evaluation of most used database engines, the {\i SQLite3} engine was found out to be secure, fast, and perfectly adapted as a stand-alone database engine for this framework, able to access other remote database engines using its unique {\i Virtual Tables} mechanism.
Together with this Client-Server data and business architecture, a set of User Interface components (especially Database Grid and Reporting system) are provided within the framework.
The main approach of this framework is to avoid @*RAD@ in the development of projects. RAD has been proved to be a good candidate about prototyping, but is not the best approach for creating a robust and maintainable application. Best practices (as MVC, n-Tier or SOA) shall be used instead.
: Expected Use
Any application which need moderate database usage (up to some GB of data) with easy setup and administration, together with a secure @*ACID@ behavior in a Client-Server environment should consider using {\i mORMot 2}.
: Requirement Exceptions
This framework was developed in order to run mainly under any {\i Delphi} compiler, from version {\i Delphi} 6 to the latest Delphi version ({\i Delphi 11 Alexandria} at time of this writing).
On the {\i server side}, it targets both {\i Win32} and {\i Win64} platforms (using the 64-bit compiler included in latest {\i Delphi} XE2 and up).
For clients, in addition to those {\i Win32} / {\i Win64} platforms, you have cross-platform code generation abilities, for any {\i Delphi} or {\i @*FreePascal@} target (including {\i @*OSX@} and mobile {\i iOS} or {\i Android}), or AJAX / HTML5 clients via {\i @*Smart Mobile Studio@} - see @90@.
\page
:Software Design Input
The Software @DI@ items follow these main divisions:
\LayoutPage

[DILayout]
; lines beginning with : will be titles for general DI layout - the 'DI-' chars are added before numbers listed below
:Units Refactoring
2.1
:Code Refactoring
2.2
:New Features
2.3

[DI-2.1]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=Units should be split into smaller scope-refined units

[DI-2.2]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=Code should be refactored

[DI-2.3]
Risk=1,1,3,Arnaud Bouchez,Initial release
Request=Initial release
Ident=New features shoold be available

[RK]
; self-owned Risk Asssessment document
Owner=RK
Order=RK
ItemName=FMEA
DisplayName=Design FMEA
Name=Design FMEA File
DocName=Design FMEA File
; this DocName will be used for generating .DOC filename (instead of ItemName)
Purpose=List {\i Failure Modes and Effects Analysis} (FMEA)
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteRisk=Yes
; Write Risk assessment table summary after every RK item
WriteTableOfContent=Yes
; Write global Table Of Contents
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Implements ISO=ISO 123456 requirements
; "Pictures" will create a table with all picture appearing in this document
; "Implements ISO=..." will create a table with all appearing "\Implements ISO 3.4" pages, with the specified item name

:Introduction
The @RK@ is a reference document used to list the {\i Failure Modes and Effects Analysis} (FMEA) identified for the {\i mORMot 2} library.
The "{\i Failure modes and effects analysis}" (FMEA) is a procedure in operations management for analysis of potential failure modes within a system for classification by severity or determination of the effect of failures on the system. {\i Failure modes} are any errors or defects in a process, design, or item, especially those that affect the customer, and can be potential or actual. {\i Effects analysis} refers to studying the consequences of those failures.
In practice, a Risk Assessment team starts with a block diagram of a system. The team then considers what happens if each block of the diagram fails, and fills in a table in which failures are paired with their effects and an evaluation of the effects. The design of the system is then corrected, and the table adjusted until the system is known not to have unacceptable problems.
This @RK@ lists most FMEA items identified as possible Software Failure for the {\i Synopse mORMot Framework}.
: Risk Assessment
In the following @RK@, a numerical Risk Assessment is given for every FMEA item, according to the {\i Risk Assessment Scale} table below.
A summary explanation is indicated, together with the names of those who made each evaluation.
\risk
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @https://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms;
- This documentation is released under the GPL (GNU General Public License) terms, without any warranty of any kind.
\page
:FMEA
: Fault Tree
Here is the Fault Tree of the framework, displayed in a graphical way:
\graph FTA mORMot Framework Fault Tree
\mORMot Framework\Framework Architecture
\Framework Architecture\Invalid Concurent Access
\Invalid Concurent Access\Database corruption
\Invalid Concurent Access\Wrong Client-Server synchro
\Wrong Client-Server synchro\Enduser problems
\Framework Architecture\Main Server Crashed
\Framework Architecture\Security issue
\Security issue\Enduser problems
\Security issue\Database corruption
\Main Server Crashed\Database corruption
\Database corruption\Enduser problems
\mORMot Framework\User Interface
\User Interface\Security issue
\User Interface\Inconsistent Layout
\Inconsistent Layout\Timeout problems
\Inconsistent Layout\Incorrect User action
\Incorrect User action\Enduser problems
\User Interface\Function not working
\Function not working\Timeout problems
\

[SRS]
Owner=DI
Order=SRS
; Owner: [SRS-*] -> * reference; Order=SRS -> [SRS-*] have sub items
;Refers=RK
; Refers will add all [SRS-RK-*] items to the list, after the DI
Name=Software Requirements Specifications
ItemName=SWRS
Purpose=Interpret design inputs and specify software design features
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
;PreparedBy=..   ignored values are taken from current
; [SRS-*] sections describe each item ([DI] items + other items)
; [SRS-*] are displayed as they appear in the file
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteRisk=Yes
; Write Risk assessment table summary after every DI
WriteTableOfContent=Yes
; Write global Table Of Contents at the end of the file
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Implements ISO=ISO 123456 requirements

:Introduction
: Documentation overview
The whole Software documentation process follows the typical steps of this diagram:
\graph FMEADI Design Inputs, FMEA and Risk Specifications
\User¤Requirements\Design Inputs¤(DI)\define
\Regulatory¤Requirements\Design Inputs¤(DI)
\Design Inputs¤(DI)\Specifications¤(SWRS)\are specified by
\System-wide¤Risk Assessment\SW FMEA¤(RK)\defines
\SW FMEA¤(RK)\Specifications¤(SWRS)
\Specifications¤(SWRS)\Architecture + Design¤(SAD+SDD)\is implemented by
\Architecture + Design¤(SAD+SDD)\Test + Documentation\is associated to
\Test + Documentation\Specifications¤(SWRS)\refers to
\
: Purpose
This @SRS@ applies to the first public release of the {\i mORMot 2} framework.
It describes the software implementation of each design input as specified by the @DI@.
The sections of this document follow the @DI@ divisions:
\LayoutPage
;Then all items created from the @RK@ are listed:
;\referspage
For each Design Input item, the corresponding justification is specified, between parenthesis (SCR #65, e.g.).
Every @SRS@ item is named about the corresponding @DI@ item, or, in case the initial {\i Design Input} is too large and must be divided into some {\i SWRS} more precise items, an unique name is proposed.
: Risk Assessment
The Risk assessment indicated below was evaluated as a team work, based on the software solution proposed.
In the following @SRS@, a numerical Risk Assessment is given for every Design Input item, according to the {\i Risk Assessment Scale} table below.
A summary explanation is indicated, together with the names of those who made each evaluation.
\risk
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @https://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms;
- This documentation is released under the GPL (GNU General Public License) terms, without any warranty of any kind.

[Risk]
Owner=SRS
Order=SRS
; Owner: [Risk-*] -> * reference; Order=SRS -> [Test-*] have no sub items
Name=Software Implementation Risk Assessment
DocName=Risk Assessment
; this DocName will be used for generating .DOC filename (instead of ItemName)
Purpose=Perform a risk assessment of the SWRS implementation
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; rev.1 summary values are in [DI] Risk=Severity,Probability,Occurence,Comment
; [Risk-*] sections contain rev.2 details for each SRS, [Risk-SER-03] or [Risk-4.5.1] e.g.
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose,RiskTable

[SAD]
Owner=SRS
Order=SRS
; Owner: [SAD-*] -> * reference; Order=SRS -> [SAD-*] have no sub items
Name=Framework Documentation
Purpose=Describe the implications of each software requirement specification on all the affected software modules
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=src\mormot.commit.inc
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; [SAD-*] sections contain details for each SRS, [SAD-SER-03] or [SAD-DI-4.10.6] e.g.
; [SAD-*] are displayed as they appear in the [SRS-*] sections
DocumentFrontPage=ProjectDetails,Warning
;DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
Source=Main,SynFile
; presence of Source=.. adds global description for each SourceFile=.. and for @Module\filename.pas@ for the project (TProject.Parse: array of TSection - [SAD-Module] will get files from @Module\name.pas@, e.g.)
SourceSDD=SDD
; name of the [SDD] section to be parsed for modified files as @!Module\bidule.pas@
DefaultPath=D:\dev\lib2
; global default directory to trim in the documentation, and used by default in [SAD-module] SourcePath=..
Directives=USE_OPENSSL;USELIBCURL;NOSYNDBZEOS
; optional global conditional define when parsing the source
WriteTableOfContent=Yes
; Write global Table Of Contents of the file
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Source,Index
; "Source" will create a table with all @source.code@ files within this document
; "Index" an index of all @*keyword@, Picture of all %pictures
WithAllfields=Yes
; write all field properties of all object and classes: document is huge, but contains all available architecture intel
FileName=mORMot2
; overwrite the default output file name

:Foreword
%happymormot.png
: Welcome to mORMot 2!
This documentation applies to the @=Revision@ revision of the {\i mORMot 2} library.
After a presentation of the framework architecture and main features, each source code unit is detailed, with clear diagrams and tables showing the dependencies between the units, and the class hierarchy of the objects implemented within.
: Getting Help
About free or professional support:
- Support is freely available in the project forum - @https://synopse.info/forum - from the {\i mORMot} Open Source community;
- Issues are better first discussed in our forum @https://synopse.info/forum/viewforum.php?id=24 before being tracked in our public repository located at @https://github.com/synopse/mORMot2/issues - without preliminary discussion in the forum, premature tickets are likely to be closed;
- You are welcome to propose some @https://github.com/synopse/mORMot2/pulls for sharing your changes of the source code;
- The source is published under a permissive @https://github.com/synopse/mORMot2/blob/master/LICENCE.md MPL/GPL/LGPL Three-License;
- Synopse, as a company, can provide a commercial license, professionnal support, expertise, bug fixes or enhancements, on request.
: Work in progress
This documentation is in early stage. API documentation is accurate, but we will try to make a new high-level presentation of the framework here.
For reference, please look at @https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html for the {\i mORMot 1} documentation, which is very verbose, but very complete, especially about architectural patterns. Most of the information still apply to {\i mORMot 2}, with some minor renaming (e.g. {\f1\fs20 @*TSQLRecord@} into {\f1\fs20 @*TOrm@}).
We would like to reduce this documentation to a startup guide, and an introduction to the framework.

[SAD-Source]
; this Section body is added as the introduction of the document first part
TitleOffset=0
DisplayName=mORMot Framework Overview

:Synopse mORMot Overview
%IamLost.png
{\i Synopse mORMot} is an Open Source @*Client-Server@ @*ORM@ @*SOA@ @*MVC@ framework for modern Object pascal, on FPC and Delphi.
The main features of {\i mORMot} are therefore:
- {\i ORM/ODM}: objects persistence on almost any database (SQL or NoSQL);
- {\i SOA}: organize your business logic into @*REST@ services, potentially defined as {\f1\fs20 interface};
- {\i Clients}: consume your data or services from any platform, via ORM classes or SOA interfaces;
- {\i Web MVC}: publish your ORM/SOA process as responsive @*Web Application@s.
All those bricks can work locally or remotely, e.g. over HTTPS or @*WebSockets@, via an auto-configuring Client-Server @*REST@ design.
\graph mORMotDesignORMSOA General mORMot architecture
subgraph cluster_0 {
"SQLDB";
label="SQL Databases";
}
subgraph cluster_1 {
"NoSQLDB";
label="NoSQL Databases";
}
subgraph cluster_2 {
"Services";
label="Services";
}
subgraph cluster_3 {
\SQLDB\ORM
\NoSQLDB\ODM
\REST Server\MVC/MVVM¤Web Server
\ORM\REST Server
\ODM\REST Server
\Services\SOA
\SOA\REST Server
label=" mORMot\nServer";
}
\REST Server\Stand Alone¤Application
subgraph cluster_4 {
label="REST   Clients";
\REST Server\... any
\REST Server\AJAX
\REST Server\Mobile
\REST Server\Delphi
}
subgraph cluster_5 {
label="    Web   Clients";
\MVC/MVVM¤Web Server\Desktop
\MVC/MVVM¤Web Server\ Mobile
}
subgraph cluster_6 {
label="         Featuring";
"Featured";
}
=SQLDB=SQLite3 - Firebird - NexusDB\nPostgreSQL - MySQL - DB2\nMS SQL - Oracle - Informix
=NoSQLDB=MongoDB\nIn-Memory\nFiles
=Services=Method-based Services\nInterface-based Services\nAsynchronous (Push) Services\nRemote (Saas) Services
=Featured=User Management - Security & Rights - Sessions - Replication¤Unit Testing - Mocks/Stubs - Logging - Performance - Profiling¤http.sys - WebSockets - MultiCore - Templates (MVC) ¤JSON - JavaScript Engine -  Reporting - PDF - UI
\
{\i mORMot} offers all features needed for building any kind of modern software project, with state-of-the-art integrated software components, designed for both completeness and complementarity, offering {\i @*convention over configuration@} solutions, and implemented for speed and efficiency.
For {\i storing some data}, you define a {\f1\fs20 class}, and the framework will take care of everything: routing, JSON marshalling, table creation, SQL generation, validation.
For {\i creating a service}, you define an {\f1\fs20 interface} and a {\f1\fs20 class}, and you are done. Of course, the same ORM/ODM or SOA methods will run on both server and client sides: code once, use everywhere!
For {\i building a MVC web site}, define the pages as {\f1\fs20 interface} methods, then write a Controller class in Delphi, then some HTML Views using {\i @*Mustache@} templates, leveraging a, existing DB, our ORM/ODM, or other SOA persistence services as Model.
If you need a HTTP server, a proxy @*redirection@, master/slave @*replication@, @*publish-subscribe@, a test, a mock, add security, define users or manage rights, a script engine, a report, User Interface, switch to XML format or publish HTML dynamic pages - just pick up the right {\f1\fs20 class} or method. If you need a tool or feature, it is probably already there, waiting for you to use it.
The table content of this document makes it clear: this is no ordinary piece of software.
Speed and scalability has been implemented from the ground up: the rewritten version 2 of the framework has pushed the performance numbers of {\i mORMot 1} even further.
In short, with {\i mORMot}, your ROI is maximized.
\page
:123 FAQ
Before you start going any further, we propose here below a simple @**FAQ@ containing the most frequent questions we received on our forums.
First of all, take a look at the {\i keyword index} available at the very beginning of this document. The underlined entries target the main article(s) about a given concept or technical term.
Feel free to give your feedback at @https://synopse.info/forum asking new questions or improving answers!
{\b Should I use {\i mORMot 2} or the older {\i mORMot 1}?}\line You are right, {\i mORMot 2} is the way to go for any new project: {\i mORMot 1} is considered in a bug-fix-only state. For an existing {\i mORMot 1} project, we will continue to fix the identified bugs and supply the {\i SQLite3} static updates, but no new feature or enhancement will appear any more on this branch. Consider migrating your project to {\i mORMot 2} as soon as you have a little time. It is not a complex process, since most of the code is compatible, once you change to the new units for your whole project.
{\b Where should I start?}\line Quickly browse this documentation, then download and install the sources @44@, then compile and run the {\f1\fs20 mormot2tests.dpr} program. Check about the various samples from the {\f1\fs20 ex} sub-folders.
{\b So far, I can see your {\i mORMot} fits most of the requirement, but seems only for Database Client-Server apps.}\line First of all, the framework is a {\i set of bricks}, so you can use it e.g. to build interface based services, even with no database at all. We tried to make its main features modular and uncoupled.
{\b I am not a great fan of ORM, sorry, I still like SQL and have some experience of that. Some times sophisticated SQL query is hard to change to ORM code.}\line ORM can make development much easier; but you can use e.g. interface-based services over existing "manual" SQL statements.
{\b I would like to replace pieces of Delphi-code by using mORMot and the @*DDD@-concept in a huge system, but its legacy database doesn't have integer primary keys, and {\i mORMot} ORM expects a {\f1\fs20 TID}-like field.}\line By design, such legacy tables are not compatible with {\i SQLite3} virtual tables, or our ORM - unless you define an {\f1\fs20 ID} integer additional primary key, which may not be the best idea. Some hints: write a {\i persistence service} as {\f1\fs20 interface}/{\f1\fs20 class}; uncouple persistence and @*SOA@ services (i.e. the SOA {\f1\fs20 @*TRestServer@} is a {\f1\fs20 @*TRestServerFullMemory@} and not a DB/ORM {\f1\fs20 @*TRestServerDB@}); reuse your existing SQL statements, with {\f1\fs20 mormot.db.*} units as access layer if possible (you will have better performance, and direct @*JSON@ support); use the ORM for @*MicroService@ local persistence (with {\i SQLite3}), and/or for new tables in your legacy DB (or another storage, e.g. @*MongoDB@).
{\b Why are you not using the latest features of the compiler, like generics or class attributes?}\line Our framework does not rely on {\i generics}, but on the power of the object pascal type system: specifying a {\f1\fs20 class} or {\f1\fs20 interface} type as parameter is safe and efficient. But we include some generic-oriented methods, and even a {\f1\fs20 {\f1\fs20 @*mormot.core.generics.pas@}} unit as an alternative to the Delphi/FPC RTL. We offer enumerates when possible, e.g. {\f1\fs20 for .. in ...} over a {\f1\fs20 @*TDocVariantData@}.
{\b I also notice in your documentation, data types are different from Delphi. You have {\f1\fs20 RawUtf8}, etc, which make me puzzled, what are they?}\line You can for sure use standard {\i Delphi} {\f1\fs20 string} type, but some more optimized types were defined: since the whole framework is @*UTF-8@ based, we encourage using the {\f1\fs20 @*RawUtf8@}={\f1\fs20 @*Utf8String@} type, which works with all versions of {\i Delphi} and FPC. By the way, just search for {\f1\fs20 RawUTF8} in the {\i keyword index} of this document.
{\b During my tests, my client receives non standard @*JSON@ with unquoted fields.}\line Internally, the framework uses JSON in {\i MongoDB} @*extended syntax@, i.e. fields are not quoted - this gives better performance and reduces memory and bandwidth with a {\i mORMot} client. To receive {\f1\fs20 "field":value} instead of {\f1\fs20 field:value}, just add a proper {\f1\fs20 @**User-Agent@} HTTP header to the client request (as any browser does), and the server will emit standard JSON.
{\b I encounter strange issues about indexes or collations with external {\i SQLite3} tools.}\line By default, our ORM uses its proprietary {\f1\fs20 SYSTENOCASE} collation, which is perfect for {\i Win1252} accents, but unknown outside of {\i mORMot}. Use our {\f1\fs20 SynDbExplorer} tool instead. Or use a standard collation when defining a new ORM table as stated below.
{\b When I work with floating points and JSON, sometimes numerical values with more than 4 decimals are converted into JSON strings.}\line By default, {\f1\fs20 double} values are disabled in the JSON serialization, to avoid any hidden precision lost during conversion: see below how to enable it.
{\b I got an access violation with mormot.db.sql ISqlDBRows.}\line You need to explicitly release the {\f1\fs20 ISqlDBRows} instance, by setting it to {\f1\fs20 nil}, {\i before} freeing the owner's connection - see below.
{\b @*Deadlock@ occurs with interface callbacks.}\line When working with asynchronous notifications over {\i WebSockets}, you need to ensure you won't fire directly a callback from a main method execution - see below for several solutions.
{\b All the objects seem non-VCL components, meaning need code each property and remember them all well.}\line This is indeed... a feature. The framework is not @*RAD@, but fully object-oriented. Thanks to the {\i Delphi} IDE, you can access all properties description via auto-completion and/or code navigation.  We tried to make the documentation exhaustive and accurate. Then you can still use RAD for UI design, but let business be abstracted in pure code. See e.g. the {\f1\fs20 mORMotVCL.pas} unit which can publish any ORM result as {\f1\fs2 TDataSource} for your UI.
{\b I know you have joined the {\i DataSnap} performance discussion and your performance won good reputation there. If I want to use your framework to replace my old project of DataSnap, how easy will it be?}\line If you used {\i DataSnap} to build method-based services, translation into {\i mORMot} will be just a matter of code refactoring. And you will benefit of new features like {\i Interface-based services} - see below - which is much more advanced than the method-based pattern, and will avoid generating the client class via a wizard, and offers additional features - see below or @72@.
{\b I am trying to search a substitute solution to WebSnap. Do you have any sample or doc to describe how to build a robust web Server?}\line You can indeed easily create a modern @*MVC@ / @*MVVM@ scaling @*Web Application@. Your {\i mORMot} server can easily publish its ORM / SOA business logic as {\i Model}, use {\i @*Mustache@} logic-less templates rendering - see below - for {\i Views}, and defining the {\i ViewModel} / {\i Controller} as regular Delphi methods. See below for more details, and discovering a sample "blog" application.
{\b Why is this framework named {\i mORMot}?}\line - Because marmots do hibernate, just like our precious objects;\line - Because marmots are highly social and use loud whistles to communicate with one another, just like our computers are designed not to be isolated;\line - Because even if they are cute little rodens eating greens, they use to fight at Spring for their realm;\line - Because it may be an acronym for "Manage Object Relational Mapping Over Territory", or whatever you may think of...
\page
:Architecture principles

[SAD-Main]
SourcePath=test
IncludePath=src;src\core;src\lib;src\crypt;src\net;src\db;src\rest;src\orm;src\soa;src\app;src\ui;src\script
SourceFile=mormot2tests.dpr;mormot.*.pas
; mormot.*.pas will find all official mORMot units in all IncludePath folders :)
SourceIgnoreSymbol=select,check,open,connect,send,sqlite3,mORMot,JavaScript,cypher,execute,cache
SourceIgnoreSymbolByUnit=SynCrossPlatformJSON,SynCrossPlatformREST,SynCrossPlatformSpecific,SynCrossPlatformTests
Version=2.0
TitleOffset=0
DisplayName=mORMot2

:Enter new territory
: Meet the mORMot
The {\i Synopse mORMot} framework consists in a huge number of units, so we will start by introducing them.
\graph mORMotSourceCodeMainUnits mORMot Source Code Main Units

[SDD]
Owner=SRS
Order=SRS
; Owner: [SDD-*] -> * reference; Order=SRS -> [SDD-*] have no sub items
Name=Software Design Document
Purpose=Summarize the software DI implementation for QA review
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; [SDD-*] sections contain details for each SRS, [SDD-SER-03] or [SDD-DI-4.10.6] e.g.
; [SDD-*] are displayed as they appear in the [SRS-*] sections
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteTableOfContent=Yes
; Write global Table Of Contents at the end of the file
; Write global Table Of Contents at the end of the file
TableOfContentsAtTheBeginning=Yes
; if the Table of Contents must be at the beginning (default=No=at the end of the file)
DocumentIndex=Pictures,Source,Index

:Introduction
: Documentation overview
The whole Software documentation process follows the typical steps of this diagram:
%%FMEADI
: Purpose
This @SDD@ applies to the release of the {\i Synopse mORMot Framework} library.
It summarizes the software implementation of each design input as specified by the @DI@.
This document is divided into the main parts of the Software implementation:
\LayoutPage
Inside this sections, source code or User Interface modifications are detailed for every @SRS@ item.
: Responsibilities
- Synopse will try to correct any identified issue;
- The Open Source community will create tickets in a public Tracker web site located at @https://synopse.info/fossil ;
- Synopse work on the framework is distributed without any warranty, according to the chosen license terms;
- This documentation is released under the GPL (GNU General Public License) terms, without any warranty of any kind.

[VV]
Owner=SRS
Order=SRS
DisplayName=V&V Plan
DocName=V&V Plan
Name=Software Validation and Verification Plan
Purpose=Define the testing required for the updates to the mORMot Framework software along with the testing responsibilities
; just the message for 'Goal:' in the Software Verification Plan section
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose
WriteTableOfContent=Yes
WriteSummaryOf=Test
; WriteSummaryOf=Test -> VV document will have 2 parts: [VV] body as introduction, then a list of all documents divided by [Test-*].Description=.., with [Test].Goal,[Test].DocName, and associated [SRS-*] (Owner=SRS) sections

:Introduction
: Purpose
This @VV@ applies to the upgrade software for the mORMot Framework, implementing the Software part of the @DI@.
Its activities cover the modifications to the software as described in the @SRS@.
: Scope
The software supplied with the instrument is divided in the following parts, as specified by the @SAD@:
...
: Risks and Contingencies
Modifications to the software are required to resolve defects or add additional approved functionality. For each new version of software the risks must be assessed if the complete V&V plan is not followed. At a minimum the following must be documented: description of change, potential impact on software, minimal testing of software that must be performed and a summary of the testing. For additional risks see the @Risk@.
: Approach
The overall approach to testing will be functionality (black box) testing. Some test will be made with specific tools (software or debugger). The comprehensiveness of the testing will be evaluated by the completion of the test summary where the features are listed.
Any software defects will be reported to the developer and added to the bug tracking database. Defects will be reviewed and resolved based on the severity, occurrence and customer impact assigned to each defect.
: Item Pass/Fail criteria
{\i Pass/Fail} criteria will be determined based on the software requirements definition.
: Test results
Test results will be documented on the test protocol summary sheet. Any defects reported during testing will also be referenced on the test protocol summary.
: Test reports
Test Reports will be compiled from the Test Plan, Procedures and Test Results.  The purposes of the Test Reports are to summarize the test protocols and results and draw a conclusion regarding the validation of the {\i Synopse mORMot Framework} to meet its design goals.
The Test Reports will contain the following information:
- Software Version Tested;
- Summary of test results;
- Summary of observations not included in this V&V plan;
- Recommendations;
- List of the features to be tested showing the test procedure used to test each specification, version tested and the pass/fail determination;
- List of each test procedure showing the date of testing, tester, pass/fail determination and reference to any defect observed in testing;
- Software Problem Reports. List all defects reported during testing with the priority and open/closed status.
: Software Verification Plan
The Test Reports are divided into several document files, as listed in the {\i Software Verification Plan} below.
The {\i Software Verification Plan} layout follows the main sections of the @DI@:
\LayoutPage
: Responsibilities
This document is intended to be reviewed by QA team.
It is the responsibility of the Software V&V person or team to:
- Follow the software V&V protocols prepared in support of this V&V plan and to document the results;
- Complete all parts of a protocol in which a selection, such as Pass or Fail is requested or explain why a determination could not be made;
- Generate a V&V report noting compliance and deviance of the measured or observed from the expected and to submit it for review.
It is the responsibility of all Software Managers, Project Manager, or their designee to review and approve the software V&V plan and the software report.
\page
:Software Verification Plan

[Test]
Owner=SRS
Order=SRS
; Owner: [Test-*] -> * reference; Order=Test -> [Test-*] have sub items
Name=Test protocols
ItemName=Test protocol
DocName=Test
Purpose=Describe all Test protocols with specific pass/fail criteria
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
; [Test-*] sections contain details for each SRS, [Test-SER-03] or [Test-DI-4.7.6] e.g.
; [Test-*] are displayed as they appear in the [SRS-*] sections
; all individual Test documents can be created with the [Tests] section
; a global 'Test protocols.doc' can also be created, containing all Test reports in one big file, with corresponding options
;WriteRisk=Yes
; global 'Test protocols.doc' will contain corresponding Risk assessment
BodyIsTest=Yes
; so any [Test-*] will have a special format: | at the beginning of the line, like |Actions[|Expected Results[|Observations]] - same as a table, but with no |% before and after, and possibly missing |
; -> text between | has to use \line to between paragraph/lines
; -> a line with only | is a separator between tests: a new table will be printed
; -> if any value is entered in 'Expected Results', a Pass/Fail message will be added in Validation
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose

:Introduction
This @Test@ regroups all the {\i Test protocols} in an unique document. It may be convenient to have all the test procedures in the same file, for review purpose, e.g.
Every @SRS@ item is listed with an abstract of its implementation, then its specific protocol is written, following the main sections the @DI@:
\LayoutPage
;The numerical Risk evaluation, as stated in @Risk@, is written again for every Design Input item, according to the {\i Risk Assessment Scale} table shown on page 2.

[Tests]
Owner=SRS
Order=SRS
Name=Test report
Purpose=Create all Tests protocols documents
; these Purpose= value will be used in the menu item hint
DocByDescription=Test
; -> individual documents divided by [Test-*].Description=.. can be created, with layout in [Tests]
; -> a last page is added, named 'Summary Sheet', taking executable versions and name in [SAD-*].Source= (SAD is TProject.ParseDoc)
SubDocFrontPage=TestDetails,RevisionDetails,AddPurpose
; additionnal front page values are taken from [Test-*].Requirements=.. and [Test-*].Notes=..
PreparedBy=Arnaud Bouchez
; // may be overwritten in [Test-*]

; body of this section is the last page summary sheet
{\ul{\b START TIME:}}    {\f1 __________________    }{\ul{\b END TIME:}}    {\f1 __________________}\line
{\ul{\b RELEVANT TESTING INFORMATION}}\line{\f1 _______________________________________________________________\line _______________________________________________________________\line _______________________________________________________________}\line
{\ul{\b DEVIATIONS FROM PROCEDURE}}\line {\f1 _______________________________________________________________\line _______________________________________________________________\line _______________________________________________________________}\line
{\ul{\b ATTACHMENTS LISTING}}
\par
\par
\par
{\b [  ]  MEETS SPECIFICATIONS}
{\b [  ]  DOES NOT MEET SPECIFICATIONS}\line
{\b PROCEDURE PERFORMED BY:\line{\f1  _____________________________________}}
{\b DATE:}{\f1  _____________}\line
{\b REVIEW AND APPROVAL BY:\line{\f1  _____________________________________}}
{\b DATE:}{\f1  _____________}

[SoftwareVersion]

:Synopse mORMot Framework
|%23%18%15%44
|\b File Name|Date|Version|Description\b0
|...|...|...|...
|%

[KnownIssues]

|%15%85
|\b Request|Description\b0
|...|...
|%

[SoftwareHistory]

:mORMot Framework
|%8%17%75
|\b Version|Date|Remarks\b0
|1.00|...|...
|%

[SCRS]
Owner=SRS
;Order=SRS
; no Order= specified, so that the body of this [SCRS] document section will be written as plain text only
Name=Software Change Request Summary Form
Purpose=Cross-reference all software changes
ItemName=SCRS
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial Version
; Revision* multiple revision Table: ignored values are taken from current, older below
YesNo=Yes
; this message will be used for 'Yes / No' value in table
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails,AddPurpose,RiskTable
WriteTableOfContent=Yes
; Write global Table Of Contents at the end of the file
TitleFlat=Yes
; so the titles will be all numerical and hierachical (without any big sections)
Landscape=Yes
; full body is to be written as Landscape

:Software Changes
\TableSoftwareChanges
:Cross References
: Reference and Related Documents
\TableDocuments
: Traceability Matrix
\TableTraceabilityMatrix
:Package Content
=[SoftwareVersion]
:Known issues
=[KnownIssues]

[Release]
Owner=DI
;Order=DI
; no Order= specified, so that the body of this [Release] document section will be written as plain text only
Name=Release Notes
Purpose=Present all software modifications introduced in the current release
PreparedBy=Arnaud Bouchez
ReviewedBy=
ApprovedBy=
Revision=2.0
RevisionDate=
RevisionDescription=Initial version
; Revision* multiple revision Table: ignored values are taken from current
DocumentFrontPage=ProjectDetails,Warning,PeopleDetails,RevisionDetails
WriteTableOfContent=Yes
; Write global Table Of Contents at the beginning of the file
TitleFlat=Yes
; so the titles will be all numerical and hierachical (without any big sections)

:Introduction
: Document Purpose
This @Release@ applies to the upgrade software for the {\i Synopse mORMot Framework}, implementing the main specifications detailed in the @DI@.
It describes the software requirements and bug corrections involved in this release.
: Software Version
This release updates the {\i mORMot Framework} software modules to the following versions.
=[SoftwareVersion]
: Software specifications
This release is compatible with the following software:
- Windows XP (or later) Operating system;
- {\i Delphi} 7 up to {\i Delphi} 2010.
:Release Notes
: New features
New features implemented in this release are listed below.
\TableNewFeatures
; all DI with Request=SCR #123 will be listed here
: Bug fixes
The following bugs reported in earlier versions have been fixed.
\TableBugFixes
; all DI with SCR #65,Module 2.0+Other Module 3.0,Low will be listed here
: Installation Instructions
The Installation of this release follows the steps detailed in the {\i mORMot Framework User Manual}, and did not change from previous version.
: Special Instructions for Use
As this release is mainly a bug fix, the instructions for use did not change, and the former {\i mORMot Framework User Manual} can be seen as a valid reference document for the User.
: Open Issues
In a general manner, the User has to follow strictly the procedures detailed in the documentation shipped with the {\i mORMot} Framework, and the corresponding software updates which may have been installed.
: Testing Status
The tests below were performed by strictly following the test protocols as stated by the @VV@, after having installed the release on a dedicated test computer.\line
\TableTests=...Date of test
All tests passed successfully. Therefore, this release should not prevent users from using features.
: Significant Faults
There are no significant remaining faults, which prevent using features.
See {\b Appendix B - Known Faults list} for the current open issues identified in the release, and are planned to be corrected.
\landscape
:Appendices
: Appendix A - Software Revision History
=[SoftwareHistory]
: Appendix B - Known Faults List
The following table lists the significant bugs that were found in this release:
=[KnownIssues]
: Appendix C - Release Documentation Audit
The following table is a partial list of related documentation, including the current revision numbers and revision dates for all documentation applicable to this release.
\TableDocuments=DI,SRS,Risk,SAD,SDD,VV,SCRS
: Appendix D - ISO 123456 cross reference
Here is a table of the implementation of the {\i ISO 123456} standard in all the documentation:
\TableImplements=ISO
At the beginning of the @RK@, @SRS@, @SAD@ and @SDD@, a dedicated table will list all {\i ISO 123456} requirements implemented in this document, with its associated page.

[SandBox]
Owner=DI
Name=SandBox
PreparedBy=Arnaud Bouchez

:Secure Communication using ECDHE
In addition to ECDSA digital signatures or ECDH-based content encryption, the {\i mORMot} framework offers a proprietary {\f1\fs20 @*ECDHE@} secure protocol for securing Client/Server communication. HTTPS/TLS should still be used with AJAX or third party endpoints. But this alternate protocol can be enabled, with both @140@ and @150@, between @*SOA@ nodes implemented with {\i mORMot} Delphi/FPC services. Advantages are easier deployment, better performance, reduced protocol complexity, and higher integration.
{\f1\fs20 SynEcc} implementation of {\f1\fs20 ECDHE} handshaking and key derivation is done in a single round trip, to avoid harmful triple handshakes, and reduce network latency. Both mutual authentication and server authentication are available, requiring a shared {\i public-key infrastructure} ({\f1\fs20 @*PKI@}) - provided e.g. by {\f1\fs20 TECCCertificateChain} - to validate exchanged certificates. Thanks to the use of ephemeral keys, handshaking features perfect forward security in its key derivation (used for encryption and message authentication). Messages transmission checks authentication, data integrity, and replay attacks, with hardware acceleration of the process, if available.
Thanks to the proven set of algorithms used, resulting security is comparable to the best TLS 1.2 configurations, without the overhead and complexity of this standard, and at very high speed.
: Mutual Authentication
To perform @*mutual authentication@, the prerequisite for each party is to have private keys ({\f1\fs20 dA} and {\f1\fs20 dB}) and public keys in certificates ({\f1\fs20 QCA} and {\f1\fs20 QCB}), hosted in a shared PKI system.
$Client (dA, QCA)                                            Server (dB, QCB)
When the client initiates the communication, it generates an ephemeral ({\f1\fs20 dE, QE}) ECC key pair (without certification), then send an identifier to the current algorithm {\f1\fs20 Algo}, a random value {\f1\fs20 RndA}, its own public key {\f1\fs20 QCA}, the ephemeral public key {\f1\fs20 QE}, concatenated and digitally signed with @*ECDSA@ using its private key {\f1\fs20 dA}.
$Client (dA, QCA)                                            Server (dB, QCB)
$
$  (dE, QE) = ECCMakeKey
$  Sign = ECDSASign(dA,sha-256(Algo|RndA|QCA|QE))
$
$    Algo|RndA|QCA|QE|Sign
$   ----------------------------------------------------------->
$
On the server side, the ECDSA signature is checked using {\f1\fs20 QCA} certificated public key, then an ephemeral ({\f1\fs20 dF, QF}) key pair is generated (without certification), and all information is sent back to the client, with a digital signature using Server private key {\f1\fs20 dB}. The signature is then validated using ECDSA on the client side, checking {\f1\fs20 QCB} certificate information.
$Client (dA, QCA)                                            Server (dB, QCB)
$
$                                                           ECDSAVerify(QCA, Sign)
$                                                           (dF, QF) = ECCMakeKey
$                                          Sign = ECDSASign(dB,sha-256(Algo|RndA|RndB|QCB|QF))
$
$                                    Algo|RndA|RndB|QCB|QF|Sign
$   <-----------------------------------------------------------
$
$  ECDSAVerify(QCB, Sign)
$
Now both ends can calculate shared secret keys {\f1\fs20 SA} and {\f1\fs20 SB}. Two session keys {\f1\fs20 kE} and {\f1\fs20 kM} are then derived using a {\f1\fs20 KDF} function (e.g. HMAC-SHA256). Subsequent {\f1\fs20 m1}, {\f1\fs20 m2}... messages will be encrypted using {\f1\fs20 kE} via an {\f1\fs20 EF} encryption function (e.g. AES256-CFB), and the current {\f1\fs20 IV} Initialization Vector, derived from the current {\f1\fs20 kM}. Finally, {\f1\fs20 kM} will authenticate them using a {\f1\fs20 MAC} function (e.g. HMAC-SHA256). {\f1\fs20 kM} value will increase as a CTR to maintain read and write sequence numbers on both sides, ensuring {\f1\fs20 IV} will change, and {\f1\fs20 MAC} won't suffer from replay attacks.
$Client (dA, QCA)                                            Server (dB, QCB)
$
$  SA = ECDH(dA,QF)                                         SA = ECDH(dF,QCA)
$  SB = ECDH(dE,QCB)                                        SB = ECDH(dB,QE)
$                  kE = KDF(SA|SB|RndA|RndB,"salt")
$                  kM = KDF(SA|SB|RndA|RndB,"hmac")
$
$   EF(kE,m1)|MAC(kM,EF(kE,m1))
$   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>
$  kM++
$                                     E(kE,m2)|MAC(kM,EF(kE,m2))
$   <+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$                                                           kM++
$   EF(kE,m3)|MAC(kM,EF(kE,m3))
$   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>
$  kM++
$    ...
$
A typical {\f1\fs20 SynEcc} implementation may use, as algorithms:
- {\f1\fs20 KDF} = HMAC-SHA256 ("salt" and "hmac" values may be customized, but known on both sides);
- {\f1\fs20 EF} = AES128-CFB or any AES mode excluding ECB, potentially in 256-bit;
- {\f1\fs20 MAC} = HMAC-SHA256 (safest), HMAC-CRC256C (fast), or combined with {\f1\fs20 EF}.
By default, the {\f1\fs20 TECDHEProtocol} class will use {\f1\fs20 kdfHmacSha256} as {\f1\fs20 KDF}, and {\f1\fs20 efAesCrc128} (i.e. AES128-CFB with combined {\f1\fs20 EF} and {\f1\fs20 MAC}), for best performance (around 700MB/s messages process thanks to hardware accelerated @*AES-NI@ and SSE4.2 {\f1\fs20 @*crc32c@} instructions).
Note that encryption is not handled at this level, since all conservative protocol implementations do not enable compression, to avoid security exploit as occured for TLS with CRIME. It is up to the application layer to process the data using e.g. {\f1\fs20 deflate} or our {\f1\fs20 @*SynLZ@} algorithm.
: Unilateral Authentication
For server-side only authentication - as is most currently implemented in regular TLS/HTTPS communications, the handshaking process is slightly reduced:
$Client                                                      Server (dB, QCB)
$
$  (dE, QE) = ECCMakeKey
$
$    Algo|RndA|QE
$   ----------------------------------------------------------->
$
$                                          Sign = ECDSASign(dB,sha-256(Algo|RndA|RndB|QCB))
$
$                                       Algo|RndA|RndB|QCB|Sign
$   <-----------------------------------------------------------
$
$  ECDSAVerify(QCB, Sign)
$  S = ECDH(dE,QCB)                                         S = ECDH(dB,QE)
$                  kE = KDF(S|RndA|RndB,"salt")
$                  kM = KDF(S|RndA|RndB,"hmac")
$
$   EF(kE,m1)|MAC(kM,EF(kE,m1))
$   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>
$  kM++
$                                     E(kE,m2)|MAC(kM,EF(kE,m2))
$   <+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$                                                           kM++
$    ...
$
In this case, the client party does not have any private/public key certification, and will compute an ephemeral {\f1\fs20 (dE, QE)} pair, which will be used using server's {\f1\fs20 CA}. The server has no mean of authenticating its client, but the connection is secured and private. The handshaking process will be slightly faster than with mutual authentication, since less ECC computing operations are performed (2 instead of 5 on the server side).
The protocol is also able to handle client-side only authentication, even if this scheme may not be very useful in practice.

