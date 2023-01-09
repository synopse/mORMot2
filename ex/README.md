# mORMot Examples

## Folder Content

This folder hosts the main Examples of the *mORMot* Open Source framework, version 2.

## Benchmarking Samples

### extdb-bench

[This sample](./extdb-bench) will benchmark the ORM + REST layer of *mORMot* over several DB backends.

By default, will validate our *SQLite3* and in-memory storage layer, either directly or as virtual tables.

You could enable additional engines by changing the conditional definitions in the main program source. Currently PostgreSQL has been validated with our internal driver - feedback is welcome for other engines!

### http-server-raw

[This sample](./http-server-raw) is a stand-alone HTTP server returning some dynamic content from GET or POST requests.

It may be used to benchmark and validate the stability of *mORMot* HTTP server on various platforms, e.g. using `ab` or `wrk` tools.

Similar to the `/plaintext` URI of [the TFB sample](#techempower-bench) below.

### lang-cmp

[This sample](./lang-cmp) is a *mORMot* conversion of "An informal comparison of several programming languages" test case.

The original code is at https://github.com/losvedir/transit-lang-cmp with C#, Typescript (Deno), Elixir, Go, Rust, and Scala versions.

Once stabilized, we may fork and include it to the official repository.

See [this blog article](https://blog.synopse.info/?post/2022/11/26/Modern-Pascal-is-Still-in-the-Race) about this sample, and how nicely it compares with C# or GoLang.

### mondgodb

[This sample](./mongodb) will benchmark the ORM + REST layer of *mORMot* over a local (or remote) *MongoDB* instance.

This code is a good entry point for what is possible with this unit in our framework, for both direct access or ORM/ODM access.
And you would be able to guess the performance numbers you may achieve with your own project.

Running a *MongoDB* database in a container is as easy as executing the following command:

    sudo docker run --name mongodb -d -p 27017:27017 mongo:latest

Then you will have a *MongoDB* server instance accessible on `localhost:27017`, so you could run the sample straight away.

### techempower-bench

Contains a [TechEmpower Framework Benchmarks](https://www.techempower.com/benchmarks) compliant implementation using *mORMot*.

It is a good example of what could be done to achieve the best performance with **mORMot**, and to compare with alternatives in some realistic scenarios. And we can be proud of our little rodent for sure - especially in respect to performance and pascal code expressiveness.

## Integrated and Advanced Samples

### mvc-blog

[MVC sample web application](./mvc-blog], publishing a simple BLOG.

It is a fully featured sample, with a MVC Web MicroService, hosting its own SQLite3 database, with Full Text search, and some mustache HTML templates.

### rest-websockets

[Demonstrate a SOA service and clients over WebSockets](./rest-websockets), using binary transfer with encryption, and callbacks.

`restws_longworkserver` and `restws_longworkclient` are the main applications. You need to first execute `restws_longworkserver`, then run as many `restws_longworkclient` instances as you want.

You will see the console output of the server logs, with all threads and events, in real time.

## ThirdPartyDemos

[The `ThirdPartyDemos` folder](./ThirdPartyDemos) contains sub-folders with some example code committed by third party *mORMot* users.

Each demo is published as pedagogical material, and with its own license terms.

Please contact each author if you find an issue in this material. Synopse is not maintaining this source code tree, but will eagerly include any pull request to enhance this folder. Feel free to propose your own sample folder!

## MPL 1.1/GPL 2.0/LGPL 2.1 three-license

The framework source code is licensed under a disjunctive three-license giving the user the choice of one of the three following sets of free software/open source licensing terms:
- *Mozilla Public License*, version 1.1 or later (MPL);
- *GNU General Public License*, version 2.0 or later (GPL);
- *GNU Lesser General Public License*, version 2.1 or later (LGPL), with *linking exception* of the *FPC modified LGPL*.
This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copy-left on code we wrote.

See [the full licensing terms](../LICENCE.md) in the root folder of this repository for more information.
