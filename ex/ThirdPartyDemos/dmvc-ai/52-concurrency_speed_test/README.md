# 52-concurrency_speed_test

Port of DMVC Framework's `concurrency_speed_test` sample to mORMot2.

## Overview

This sample demonstrates concurrent request handling and performance testing with mORMot2's built-in thread pooling capabilities.

## Features

- **Thread Pool**: Uses mORMot2's thread pool (32 threads) for concurrent request handling
- **Minimal Overhead**: Simple endpoint returning a single character to maximize throughput
- **Performance Testing**: Designed for load testing with tools like Apache Bench, wrk, or Locust
- **CORS Enabled**: Access-Control-Allow-Origin: * for testing from any origin

## API Endpoints

| Method | Path   | Description                        |
|--------|--------|------------------------------------|
| GET    | `/api` | Returns 'X' (minimal test endpoint)|

## Running the Sample

1. Compile and run the server:
   ```bash
   52-concurrency_speed_test.exe
   ```

2. The server will start on http://localhost:9999

3. Test the endpoint:
   ```bash
   curl http://localhost:9999/api
   ```

## Load Testing

### Apache Bench
```bash
# 10,000 requests with 100 concurrent connections
ab -n 10000 -c 100 http://localhost:9999/api

# 100,000 requests with 500 concurrent connections
ab -n 100000 -c 500 http://localhost:9999/api
```

### wrk
```bash
# 10 threads, 100 connections, 30 seconds
wrk -t10 -c100 -d30s http://localhost:9999/api

# 12 threads, 400 connections, 1 minute
wrk -t12 -c400 -d60s http://localhost:9999/api
```

### Locust (Python)
A Locust test file is included in the `locusttest/` directory:

```bash
cd locusttest
locust -f locustfile.py --host=http://localhost:9999
```

Then open http://localhost:8089 in your browser to configure and run the test.

## mORMot2 Concurrency Features

### Thread Pooling
- The server uses `threaded=32` parameter to create a thread pool
- Each incoming request is handled by an available thread from the pool
- This provides efficient concurrent request handling without spawning new threads per request

### In-Memory REST Server
- Uses `TRestServerFullMemory` for maximum performance
- No database overhead for this simple test
- All processing is in-memory

### Service Interface
- Uses mORMot2's interface-based services
- Method-based JSON-RPC over HTTP
- Automatic JSON serialization/deserialization

## Original DMVC Sample

**Source**: `DMVCFramework/samples/concurrency_speed_test/`

**Key Differences**:
- DMVC uses Indy HTTP server with MaxConnections and ListenQueue settings
- mORMot2 uses built-in HTTP server with thread pool
- DMVC uses controller-based routing
- mORMot2 uses interface-based services

## Performance Expectations

mORMot2's HTTP server is highly optimized for performance:
- Minimal memory allocation per request
- Efficient thread pool management
- Zero-copy JSON processing where possible
- Low-latency request handling

Expected throughput (depends on hardware):
- 10,000+ requests/second on modern hardware
- Sub-millisecond response times
- Linear scaling with concurrent connections (up to thread pool size)

## Files

```
52-concurrency_speed_test/
├── 52-concurrency_speed_test.dpr      # Main program
├── 52-concurrency_speed_test.dproj    # Delphi project
├── README.md                          # This file
├── src/
│   ├── server.pas                     # HTTP server setup
│   ├── api.interfaces.pas             # Service interface
│   └── api.impl.pas                   # Service implementation
└── locusttest/
    ├── locustfile.py                  # Locust test script
    └── run.bat                        # Locust runner
```

## See Also

- [01-basicdemo_server](../01-basicdemo_server/) - Basic mORMot2 server
- [46-profiling](../46-profiling/) - Performance profiling sample
- [47-profiling_showcase](../47-profiling_showcase/) - Advanced profiling

## License

Same as mORMot2 framework (MPL 1.1/GPL 2.0/LGPL 2.1)
