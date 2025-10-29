# tdd-service

Test-Driven-Development Service, publishing a simple service, following the TDD and Clean-Architecture patterns.

The source code was made and first shown during a WorkShop at [EKON 29](https://entwickler-konferenz.de/en/). It features a stand-alone REST service, but illustrating some good practice patterns, like TDD, Clean Architecture, KDD/DDD.

You may follow the initial commits history to find out a proposed way of implementing such pattern.

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

