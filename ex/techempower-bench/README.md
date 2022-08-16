# mORMot Benchmarking Test

[TechEmpower Framework Benchmarks](https://www.techempower.com/benchmarks) (TFB) is a performance comparison of many web application frameworks executing fundamental tasks such as JSON serialization, database access, and server-side template composition. Each framework is operating in a realistic production configuration. Results are captured on cloud instances and on physical hardware. The test implementations are largely community-contributed and all source is available at [their GitHub repository](https://github.com/TechEmpower/FrameworkBenchmarks).

This folder contains a TFB implementation using the [mORMot2](https://github.com/synopse/mORMot2) FreePascal/Delphi framework.
It builds using [FreePascal](https://www.freepascal.org/) compiler and developed using [Lazarus IDE](https://www.lazarus-ide.org/)

 - [raw.pas](src/raw.pas) is a low-level implementation with or without ORM.
 
 
### Test Type Implementation Source Code

* [Raw implementation for all tests](src/raw.pas)

## Important Libraries
The tests were run with:
* [mORMot2 latest](https://github.com/synopse/mORMot2)
* [FreePascal 3.2.2](https://www.freepascal.org/)

## Contributor tips
For debugging purpose run Postges using Docker
```shell
sudo docker run --name postgres -e POSTGRES_PASSWORD=postgres -d -p 5432:5432 postgres:12
```
add `tfb-database` into hosts
```shell
echo '127.0.0.1	 tfb-database' | sudo tee -a /etc/hosts
```

Database can be initialized using the supplied scripts from [TFBDatabases repo](https://github.com/TechEmpower/TFBDatabases)

```shell
psql postgres://postgres:postgres@tfb-database:5432 < create-postgres-database.sql
psql postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world < create-postgres.sql
```

Those two `.sql` scripts are located in this folder for convenience.

## Some Numbers

This file is currently in a pull request of the TFB repository.

In the meanwhile, we ran our own tests, with comparison with the fastest known frameworks. 

We profiled the mORMot framework code, and made some improvements, visible in the following numbers (taken from 2022/07 to 2022/08):

```
┌--------------┬------------┬------------┬------------┬------------┬------------┐------------┐------------┐
│   (index)    │mormot(0720)│mormot(0730)│mormot(0801)│mormot(0802)│mormot(0813)│ drogon     │ lithium    │
├--------------┼------------┼------------┼------------┼------------┼------------┤------------┤------------┤
│   fortune    │   74318    │   90500    │   91287    │   113073   │   126055   │   176131   │   90064    │
│  plaintext   │   920198   │   977024   │   986253   │  1436231   │  1373177   │  3583444   │  3388906   │
│      db      │   111119   │   116756   │   117624   │   153009   │   154033   │   176776   │   99463    │
│    update    │   10177    │   10108    │   10981    │   15476    │   15336    │   90230    │   25718    │
│     json     │   422771   │   446284   │   458358   │   590979   │   584294   │   554328   │   544247   │
│    query     │   106665   │   113516   │   114842   │   148187   │   149122   │   171092   │   94638    │
│ cached-query │   384818   │   416903   │   419020   │   547307   │   551230   │            │   528433   │
└--------------┴------------┴------------┴------------┴------------┴------------┘------------┘------------┘
```

Note that drogon and lithium are not full ORM frameworks. They are C++ templates engines, with pre-generated code. So they don't use RTTI or a separated Mustache template as mORMot. The *fortune* test is interresting: it runs a query on PostgreSQL of several lines using the mORMot ORM, then add a line at runtime, sort the items by name, then run it using a Mustache template - over a HTTP kept-alive connection. This is certainly a realistic approach.

Here is the implementation of the *fortunes* test, which makes direct use of the mORMot ORM, *TDynArray* wrapper, and *Mustache* template system:

```
function TRawAsyncServer.fortunes(ctxt: THttpServerRequestAbstract): cardinal;
var
  list: TOrmFortunes;
  new: TOrmFortune;
  arr: TDynArray;
begin
  result := HTTP_SERVERERROR;
  arr.Init(TypeInfo(TOrmFortunes), list);
  if fStore.Orm.RetrieveListObjArray(list, TOrmFortune, '', []) then
    try
      new := TOrmFortune.Create;
      new.Message := FORTUNES_MESSAGE;
      arr.Add(new);
      arr.Sort(OrmFortuneCompareByMessage);
      ctxt.OutContent := fTemplate.RenderDataArray(arr);
      ctxt.OutContentType := HTML_CONTENT_TYPE;
      result := HTTP_SUCCESS;
    finally
      arr.Clear;
    end;
end;
```

This above code sounds pretty readable. Much more readable for sure than the C++ alternatives.

Also note that those tests use PostgreSQL. In most mORMot configurations, a typical MicroService would rather use its own embedded SQLite3 database. And here, the numbers are twice higher. So in fact, a production-ready mORMot service with its stand-alone database is likely to blow away any other frameworks using a separated PostgreSQL database. As such, the *cached-query* test, which returns some items using the DB/ORM cache, is a typical workload on a production system, and *mORMot* shines in this test.

## Test URLs

See [this TFB reference page](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview) about the corresponding requirements of each test URL.

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

Note: `mORMot` HTTP server does not support [HTTP pipelining](https://developer.mozilla.org/en-US/docs/Web/HTTP/Connection_management_in_HTTP_1.x#http_pipelining),
so numbers is not so impressive here. But pipelining is clearly something I have never seen on production. The other tests, e.g.  the `cached_query` test are more representative.

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes

