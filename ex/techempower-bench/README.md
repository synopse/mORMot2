# mORMot 2 TFB Benchmarking Test

[TechEmpower Framework Benchmarks](https://www.techempower.com/benchmarks) (TFB) is a performance comparison of many web application frameworks executing fundamental tasks such as JSON serialization, database access, and server-side template composition. Each framework is operating in a realistic production configuration. Results are captured on cloud instances and on physical hardware. The test implementations are largely community-contributed and all source is available at [their GitHub repository](https://github.com/TechEmpower/FrameworkBenchmarks).

This folder contains a TFB implementation using the [mORMot2](https://github.com/synopse/mORMot2) FreePascal/Delphi framework.
It builds using [FreePascal](https://www.freepascal.org/) compiler and was developed using [Lazarus IDE](https://www.lazarus-ide.org/)
It mimics the official version, [integrated into the TFB repository](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Pascal/mormot).

The [supplied server program](raw.pas) publishes three families of endpoints, defined as such in the `"display_name"` of the benchmark configuration:
* `mormot [orm]` using the ORM layer;
* `mormot [direct]` using the direct DB layer - mapping `/raw*` endpoints;
* `mormot [async]` using the asynchronous DB layer (only available on PostgreSQL by now) - mapping `/async*` endpoints;
 
### Test Type Implementation Source Code

[Raw implementation for all tests](raw.pas) with or without ORM.

## Important Libraries
The tests were run with:
* [mORMot2 latest](https://github.com/synopse/mORMot2)
* [FreePascal 3.2.2](https://www.freepascal.org/)

## Contributor tips
For debugging purpose run PostgreSQL using Docker:
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

On high-end hardware, you may need to change the configuration, to increase `max_connection`:
```shell
sudo docker cp postgres:/var/lib/postgresql/data/postgresql.conf .
sudo nano postgresql.conf
sudo docker cp postgresql.conf postgres:/var/lib/postgresql/data
```
Otherwise, you may encounter some "too many connections" errors during testing.


## Command line  parameters

Working threads (per server), servers count and CPU pinning can be specified in command line as such:
```
$ raw [-s serversCount] [-t threadsPerServer] [-p] 
```
Default values are computed at startup depending on the accessible (can be limited using `taskset`) CPU cores
on the running system - trying to leverage the framework potential.

Example (consider total CPU count > 12):
```shell
# use all CPUs and default parameters
./raw

# limit to first 6 CPUs and use default parameters
taskset -c 0-5 ./raw

# limit to first 6 CPUs, launch 6 servers with 4 threads for each without pinning servers to CPUs
taskset -c 0-5 ./raw -s 6 -t 4
```
Depending on the hardware you have, you may consider using our [x86_64 Memory Manager](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.fpcx64mm.pas) if your CPU has less than 8/16 cores, but would rather switch to [the libc Memory Manager](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.fpclibcmm.pas) for high-end harware. On the TFB hardware, we enable the libc heap, which has lower performance with a few cores, but scales better when allocating small blocks with a high number of cores.

## Some Numbers

As reference, the current status of the TFB challenge internal rounds is available at https://tfb-status.techempower.com

Some discussion, with updated numbers after each framework tuning, is available [in the Synopse forum website](https://synopse.info/forum/viewtopic.php?id=6443).

In practice, *mORMot* numbers are within the top #20 of all frameworks, among 296 tested. So pascal as a language is still in the race. :)

**mORMot is within the top #3 frameworks with a full ORM**. Note that drogon and lithium are not full ORM frameworks. They are C++ templates engines, with pre-generated code. So they don't use RTTI or a separated Mustache template as *mORMot*.

Also note that those tests use PostgreSQL. In most *mORMot* configurations, a typical MicroService would rather use its own embedded SQLite3 database. And here, the numbers are twice higher. So in fact, a production-ready *mORMot* service with its stand-alone database is likely to blow away any other framework using a separated PostgreSQL database. As such, the [cached-queries](#cached-queries) test, which returns some items using the DB/ORM cache, is a typical workload on a production system, and *mORMot* shines in this test. It is currently the first full ORM listed.

## Test URLs

See [this TFB reference page](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview) about the corresponding requirements of each test URL. In short, the DB queries should be serialized (data rows should be requested one by one), and some explicit steps are detailed during each process.

The plain `/*` URI are using the ORM, and the `/raw*` and `/async*` URI have a direct access to the DB layer without the ORM, and tuned SQL using PostgreSQL in pipelined mode, so are faster.

You can reproduce most TFB tests by using the [wrk tool](https://github.com/wg/wrk), on Linux. Here are some typical command lines:
```shell
# mormot [orm] endpoints
wrk -c 512 -t 3 -d 5 http://localhost:8080/plaintext
wrk -c 512 -t 3 -d 5 http://localhost:8080/plaintext -s pipeline.lua -- 16
wrk -c 512 -t 3 -d 5 http://localhost:8080/json
wrk -c 512 -t 3 -d 5 http://localhost:8080/db
wrk -c 512 -t 3 -d 5 http://localhost:8080/queries
wrk -c 512 -t 3 -d 5 http://localhost:8080/queries?queries=20
wrk -c 512 -t 3 -d 5 http://localhost:8080/cached-queries?count=100
wrk -c 512 -t 3 -d 5 http://localhost:8080/updates?queries=20
wrk -c 512 -t 3 -d 5 http://localhost:8080/fortunes
# mormot [direct] endpoints
wrk -c 512 -t 3 -d 5 http://localhost:8080/rawdb
wrk -c 512 -t 3 -d 5 http://localhost:8080/rawqueries
wrk -c 512 -t 3 -d 5 http://localhost:8080/rawqueries?queries=20
wrk -c 512 -t 3 -d 5 http://localhost:8080/rawcached?count=100
wrk -c 512 -t 3 -d 5 http://localhost:8080/rawupdates?queries=20
wrk -c 512 -t 3 -d 5 http://localhost:8080/rawfortunes
# mormot [async] endpoints
wrk -c 512 -t 3 -d 5 http://localhost:8080/asyncdb
wrk -c 512 -t 3 -d 5 http://localhost:8080/asyncqueries
wrk -c 512 -t 3 -d 5 http://localhost:8080/asyncqueries?queries=20
wrk -c 512 -t 3 -d 5 http://localhost:8080/asyncupdates?queries=20
wrk -c 512 -t 3 -d 5 http://localhost:8080/asyncfortunes
```
where `pipeline.lua` is
```lua
init = function(args)
  local r = {}
  local depth = tonumber(args[1]) or 1
  for i=1,depth do
    r[i] = wrk.format()
  end
  req = table.concat(r)
end

request = function()
  return req
end
```
For instance, you can stress the *mORMot 2* new event-driven (async) Web Server by using 16,384 concurrent connections:
```
$ ulimit -n 40000

$ wrk -c 16384 -t 3 -d 15 http://localhost:8080/plaintext -s pipeline.lua -- 16
Running 15s test @ http://localhost:8080/plaintext
  3 threads and 16384 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    79.45ms   42.01ms 442.43ms   51.90%
    Req/Sec   343.91k   148.00k  728.32k    65.58%
  12112848 requests in 15.09s, 1.85GB read
Requests/sec: 802682.65
Transfer/sec:    125.54MB

$ wrk -c 16384 -t 3 -d 15 http://localhost:8080/plaintext
Running 15s test @ http://localhost:8080/plaintext
  3 threads and 16384 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    69.83ms   26.50ms 233.33ms   74.04%
    Req/Sec    40.98k    13.39k   86.03k    73.23%
  1486190 requests in 15.04s, 232.44MB read
Requests/sec:  98841.92
Transfer/sec:     15.46MB
```
Those numbers are taken on my old TP470 Core i5 laptop on Debian 11, consuming 150MB of RAM with our [x86_64 asm MM](https://github.com/synopse/mORMot2/blob/master/src/core/mormot.core.fpcx64mm.pas). I don't think any other Delphi or FPC web server could sustain those numbers. In pipelined mode, it achieves more than 800,000 requests per second over 16,364 concurrent connections. On Linux, don't forget to call first `ulimit` to open so many sockets at once.

### JSON

**JSON Serialization**: Exercises the framework fundamentals including keep-alive support, request routing, request header parsing, object instantiation, JSON serialization, response header generation, and request count throughput.

```
http://localhost:8080/json
```

We use a `record` and regular *mORMot* JSON serialization, which its custom RTTI, for this endpoint.

### PLAINTEXT

**Plaintext**: An exercise of the request-routing fundamentals only, designed to demonstrate the capacity of high-performance platforms in particular. Requests will be sent using HTTP pipelining. The response payload is still small, meaning good performance is still necessary in order to saturate the gigabit Ethernet of the test environment.

```
http://localhost:8080/plaintext
```

Notice: the latest *mORMot 2* event-driven HTTP server does support [HTTP pipelining](https://developer.mozilla.org/en-US/docs/Web/HTTP/Connection_management_in_HTTP_1.x#http_pipelining), so numbers are very good, very close to the best frameworks - in fact, the 2.5Gb ethernet link of TFB hardware seems saturated. See [above](#test-urls) how to make pipelined queries with `wrk`.
But note that pipelining is clearly something we should not see on production, for security and stability reasons.

### DB

**Single Database Query**: Exercises the framework's object-relational mapper (ORM), random number generator, database driver, and database connection pool.

```
http://localhost:8080/db
http://localhost:8080/rawdb
http://localhost:8080/asyncdb
```

On PostgreSQL, `/asyncdb` uses pipelined mode, so is faster than plain `/db` and `/rawdb`, which emit a standard blocking `SELECT` request.

### QUERIES

**Multiple Database Queries**: A variation of the [previous Test](#db), also using the same World table. Multiple rows are fetched to more dramatically punish the database driver and connection pool. At the highest queries-per-request tested (20), this test demonstrates all frameworks' convergence toward zero requests-per-second as database activity increases.

```
http://localhost:8080/queries?queries=##
http://localhost:8080/rawqueries?queries=##
http://localhost:8080/asyncqueries?queries=##
```

On PostgreSQL, both `/rawqueries` and `/asyncqueries` use pipelined requests, so are faster than the ORM `/queries`, which emits a standard blocking `SELECT` request.

### CACHED QUERIES

**Caching**: Exercises the platform or framework's in-memory caching of information sourced from a database. For implementation simplicity, the requirements are very similar to the [multiple database query test](#queries), but use a separate database table and are fairly generous/forgiving, allowing for each platform or framework's best practices to be applied.

```
http://localhost:8080/cached-queries?count=##
http://localhost:8080/rawcached?count=##
```

Note that here, the number of objects retrieved is specified as `?count=##` and not as `?queries=##` as with other DB requests - this is as specified by TFB requirements.

The plain `/cached-queries` uses the regular ORM cache, and `/rawcached` uses a simple in-memory array cache (which is not perhaps fully compliant with TFB requirements).

### UPDATE

**Database Updates**: A variation of the [multiple database query test](#queries) that exercises the ORM's persistence of objects and the database driver's performance at running UPDATE statements or similar. The spirit of this test is to exercise a variable number of read-then-write style database operations.

```
http://localhost:8080/update?queries=##
http://localhost:8080/rawupdate?queries=##
http://localhost:8080/asyncupdate?queries=##
```

Some notes:
* `/update` endpoint uses regular *mORMot* ORM, which generates a single `UPDATE` with array parameters binding, then `UNNEST` for `?queries > 1`.
* `/rawupdate` is written with two algorithms: `UPDATE ... CASE ... THEN .. WHERE` up to `?queries=20` (this is the weird but efficient syntax as used by the faster frameworks), then `UNNEST` above 20 objects.
* `/asyncupdate` only uses the `UPDATE ... CASE ... THEN .. WHERE` kind of statement and is only marginally faster.
* both `/rawupdate` and `/asyncupdate` make pipelined requests for their `SELECT` first part, as during the [multiple database query test](#queries).

### FORTUNES

**Fortunes**: Exercises the ORM, database connectivity, dynamic-size collections, sorting, server-side templates, XSS countermeasures, and character encoding.

```
http://localhost:8080/fortunes
http://localhost:8080/rawfortunes
http://localhost:8080/asyncfortunes
```

This *fortune* test is interresting: it runs a query on PostgreSQL of several lines using the *mORMot* ORM, then add a line at runtime, sort the items by name, then run it using a Mustache template - over a HTTP kept-alive connection. This is certainly a realistic approach.

Both implementations leverage `mormot.core.mustache` over an in-memory `TDynArray`. Here is the implementation of the `/fortunes` test, which makes direct use of the *mORMot* ORM, *TDynArray* wrapper, and *Mustache* template system:
```pascal
function TasyncAsyncServer.fortunes(ctxt: THttpServerRequest): cardinal;
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
This above code sounds pretty readable. Much more readable for sure than the C++ or Rust alternatives, or even C# asp.net circumvoluted code.

On PostgreSQL, `/asyncfortunes` uses pipelined mode, so is faster than plain `/fortunes` and `/rawfortunes`, which emit a standard blocking `SELECT` request.
