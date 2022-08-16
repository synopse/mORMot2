# mORMot Examples

## Folder Content

This folder hosts the main Examples of the *mORMot* Open Source framework, version 2.

### extdb-bench

This sample will benchmark the ORM + REST layer of mORMot over several DB backends.

By default, will validate our *SQLite3* and in-memory storage layer, either directly or as virtual tables.

You could enable additional engines by changing the conditional definitions in the main program source. Currently PostgreSQL has been validated with our internal driver - feedback is welcome for other engines!

### mondgodb

This sample will benchmark the ORM + REST layer of mORMot over a local (or remote) MongoDB instance.

This code is a good entry point for what is possible with this unit in our framework, for both direct access or ORM/ODM access.
And you would be able to guess the performance numbers you may achieve with your own project.

Running a MongoDB database in a container is as easy as executing the following command:

    sudo docker run --name mongodb -d -p 27017:27017 mongo:latest

Then you will have a MongoDB server instance accessible on `localhost:27017`, so you could run the sample straight away.

### mvc-blog

MVC sample web application, publishing a simple BLOG.

It is a fully featured sample, with a MVC Web MicroService, hosting its own SQLite3 database, with Full Text search, and some mustache HTML templates.

### rest-websockets

Demonstrate a SOA service and clients over WebSockets, using binary transfer with encryption, and callbacks.

`restws_longworkserver` and `restws_longworkclient` are the main applications. You need to first execute `restws_longworkserver`, then run as many `restws_longworkclient` instances as you want.

You will see the console output of the server logs, with all threads and events, in real time.

### techempower-bench

Contains a [TechEmpower Framework Benchmarks](https://www.techempower.com/benchmarks) compliant implementation using *mORMot*.

It is a good example of what could be done to achieve the best performance with *mORMot*, and to compare with alternatives in some realistic scenarios. And we can be proud of our little rodent for sure - especially in respect to performance and pascal code expressiveness.

### ThirdPartyDemos

This folder contains sub-folders with some example code committed by third party mORMot users.

Each demo is published as pedagogical material.

## MPL 1.1/GPL 2.0/LGPL 2.1 three-license

The framework source code is licensed under a disjunctive three-license giving the user the choice of one of the three following sets of free software/open source licensing terms:
- *Mozilla Public License*, version 1.1 or later (MPL);
- *GNU General Public License*, version 2.0 or later (GPL);
- *GNU Lesser General Public License*, version 2.1 or later (LGPL), with *linking exception* of the *FPC modified LGPL*.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copy-left on code we wrote.

See [the full licensing terms](../LICENCE.md) in the root folder of this repository for more information.
