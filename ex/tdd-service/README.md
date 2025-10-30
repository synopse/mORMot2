# tdd-service

Test-Driven-Development sample, publishing a simple service, following the Clean-Architecture pattern

The source code was made and first shown during a Workshop at [EKON 29](https://entwickler-konferenz.de/en/). It features a stand-alone REST service, but illustrates some good practice patterns, like TDD, Clean Architecture, KDD/DDD and IoC.
You can look at the [Slides in SlideShare](https://www.slideshare.net/slideshow/workshop-about-mormot-2-during-ekon-write-a-clean-architecture-tdd-sample/283972835).

This document will try to give you some clues about how to read and learn from this code sample.

## Step by Step

You may first follow the initial commits history to find out our way of implementing such pattern. Note that it is only a proposal, from our very own opinionated coding experience, trying to follow best practices since years. We just hope you may pick up some ideas from this coding process.

- Step 1: Put together the Tests.
https://github.com/synopse/mORMot2/commit/b0791d127

- Step 2: Test & define some Domain/Business entities.
https://github.com/synopse/mORMot2/commit/30ab6bfe0

- Step 3: Test & define the Mobile API interface.
https://github.com/synopse/mORMot2/commit/402a62919

- Step 4: Define the Persistence infrastructure interface and implement the Mobile API.
https://github.com/synopse/mORMot2/commit/51b45e259

- Step 5: Implement the Persistence using ORM.
https://github.com/synopse/mORMot2/commit/eeefe9d61

- Step 6: Complete the Mobile API, and the needed implementation.
https://github.com/synopse/mORMot2/commit/bf55ca3b7

- Step 7: Test and prepare a daemon/service container.

- Step 8: Make a daemon/service stand-alone executable.


## Followed Patterns

### Code First

This may be the main point of surprise, if you come from a classical Delphi RAD way of development.

#### Code First with No Component

Like most modern Enterprise level toolboxes in the industry (e.g. in DotNet, GoLang or Java), *mORMot* has no IDE component. It is only source code, and your project, at least in its internal server logic, will be only source code. Only at the client presentation layer, some UI components would arise - and they should, because for UI designing, nothing compares to the good old Delphi/Lazarus RAD approach.

But the logic will stay in pure source code, with proper testing (see next point) and safe maintainability.

#### Code First, Database Last

In the RAD approach, we coders do usually start from the database tables and columns. We try to write some SQL statements to get or set the new data as needed. Then we define the UI, which consumes this information.

A more modern approach is to abstract the database. In this sample, we would define a "persistence" abstract service, so we will start from the uses cases (by writing their test), and we will eventually work about the database itself - but only in the step 5 of our process.

### Test Driven Development

Test first, fail, implement, fail, debug, pass, repeat. This is what TDD is about, in a nutshell.

If you followed the steps above, you did observe this pattern in practice.
When you write some new code, even if you are extending/maintaining a legacy application, you could certainly define a "software seam" as one or several pascal `interface`(s) to link your new code to the existing code base.

Similarly, the actual server daemon/service, with its HTTP/REST endpoint will appear last (step 8 above). We will have our code logic properly tested, and once it is validated, we will publish it to be consumed by actual clients. But this will be the cherry on the cake, and the client development could be safely delayed (or delegated to a third party company) until the end of the development cycle.

### Clean Architecture

From the *physical* point of view, most services use a 3 layers architecture: database, server, client.
But from the *logical* point of view - i.e. the code logic itself, we may take care of not making to much dependencies between the code areas. This is what this pattern is about.

#### Domain Core

The "Clean Architecture" - also used in Domain-Driven-Design - is not written as horizontal layers, but as onion-like layers. The idea is to keep the domain business as clean as possible, in the "Domain Core".
This core would contain the main domain entities (aka objects) of your business logic. Then it would define some `interface` for their dependencies (e.g. DB/persistence). In practice, those dependencies will be implemented not in the core, but in the outer layer of the onion, in the so-called "infrastructure" part of the code.

Here, naming and strong typing make a lot of sense. In the Domain Driven Design pattern, the Ubiquitous language and the Domain Boudaries patterns would take place here. In practice, this is where the most critical of your code will be hosted. This is where your company value and knowledge will be kept. Consider it as the "code vault" where you keep your trade secrets, where you want your legacy be kept as clean as possible, unpolluted from technical details like SQL or legacy code plumbing.

#### Infrastructure Layer

For instance, it may be a good practice to define a "persistence" `interface`, which may access your existing legacy DB on production using SQL, but may be implemented using a fake stub/mock or a local (in-memory) SQLite3 database for logic testing.

In this sample, we will define such an `interface`, and rely on our ORM over an in-memory SQLite3 engine. But you could implement this `interface` with another class, and connect to an existing DB and write the requests as SQL. Then switch during compilation or at runtime to the proper implementation class, without touching the other layers.

In practice, this "Infrastructure" area may contain most of your actual code base, some part of it reusing legacy code and SQL, some part even written by calling another service implemented outside your company, or developed in another language - see `mormot.net.openapi.pas` to easily generate client code in object pascal for such external dependencies.
Database is just what we used in our sample, because it is the most obvious dependency. But you may implement here some third-party services, legacy calculation code, SMS or email sending, ERP gateways... whatever is not part of your Domain Core.
In this area, code could get ugly, with some reminiscence of decades old pascal legacy, or hiding some dependencies you don't even dare to talk anyone about. 

#### Application and Presentation Layers

These outer layers are where the actual end-user workflow is defined. In practice, it will use the entities and `interface` defined in the Domain Core, and consume them to produce the expected result, following the use case defined by the domain and UI experts.

We will try to define our mobile API first, even before the dependencies (e.g. persistence) are available. It will help reduce the scope of what will be actually implemented to only what is needed. Another benefit of defining these APIs first, may be the possibility to deliver the endpoints definition soon to a client third-party, e.g. a web agency: you could make a "fake" server with fake data, and let another team work on the client/UI side, then you will eventually (and safely) switch to the final code, once done.

In this sample, we will use a single Layer for both "Application" and "Presentation" APIs. But in more complex systems, you may consider splitting the workflow into two layers, e.g. two presentation APIs, one for a VCL/LCL rich (monolithic) client and one for a Web application. Each presentation API will follow the exact use cases of both clients: methods will follow screen navigation, and parameters/DTOs will follow the screen content. So the rich and web presentation APIs will be diverse, and optimized. But both will share the same Application Layer, where the actual business workflow will be defined.

As you will see in this codebase, we don't leak the "Domain Core" entities to the presentation layers. We define custom types and `record` in the application API level. It is on purpose, mainly to reduce the extent of information to only what is actually needed on the client side, but also to reduce dependencies between the client API and the main Domain core. Using such types - known as Data Transfer Objects (DTOs) is one of the most important pattern of Clean Architecture (and TDD).

### IoC

Inversion of Concerns, or Dependency Injection, is the main (and sometimes tricky) point of `interface` usage. In this example, we will first use direct injection at constructor/`Create` level. But on real more complex systems, you may follow the factory or resolver patterns, using what *mORMot* offers, or with you own dependency injection mechanism.

