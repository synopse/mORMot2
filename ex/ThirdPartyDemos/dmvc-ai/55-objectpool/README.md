# 55-objectpool - Object Pooling Pattern

**Port of**: DMVCFramework `samples/objectpool`
**Difficulty**: Intermediate
**Demonstrates**: Object pooling patterns for performance optimization

## Overview

This sample demonstrates object pooling patterns to reduce memory allocation overhead and improve performance. Object pooling is essential when:

- Object creation is expensive (initialization overhead)
- Objects are used frequently but briefly
- Memory allocation/deallocation creates performance bottlenecks
- You want to limit resource usage (max concurrent objects)

The sample implements a thread-safe worker pool that manages expensive worker objects, reusing them across requests instead of creating/destroying on each use.

## Key Concepts

### What is Object Pooling?

Object pooling maintains a pool of pre-initialized objects that can be borrowed for use and returned when done. Benefits include:

1. **Reduced Allocation Overhead**: Objects are created once and reused
2. **Predictable Performance**: No GC pauses from frequent allocation/deallocation
3. **Resource Control**: Limit maximum concurrent instances
4. **Improved Throughput**: Less time spent in object lifecycle management

### When to Use Object Pooling

✅ **Good Use Cases**:
- Database connections
- Thread workers
- Large buffers or data structures
- Network sockets
- Parsers or processors with expensive initialization

❌ **Poor Use Cases**:
- Simple objects (strings, integers)
- Objects with complex state that's hard to reset
- Rarely used objects
- Objects that must be unique per request

## Architecture

```
┌─────────────┐
│   Client    │
│  (HTTP API) │
└──────┬──────┘
       │
       v
┌──────────────────┐      ┌──────────────┐
│ TObjectPoolApi   │◄────►│ TWorkerPool  │
│  (API Service)   │      │ (Pool Mgmt)  │
└──────────────────┘      └──────┬───────┘
                                 │
                          ┌──────┴────────┐
                          │  Available    │
                          │  Workers      │
                          │ (Locked List) │
                          └───────────────┘
                                 │
                    ┌────────────┼────────────┐
                    v            v            v
              ┌──────────┐ ┌──────────┐ ┌──────────┐
              │ Worker#1 │ │ Worker#2 │ │ Worker#N │
              └──────────┘ └──────────┘ └──────────┘
```

## Implementation Details

### 1. Worker Class (`worker.pas`)

The `TExpensiveWorker` simulates an object with expensive initialization:

```pascal
TExpensiveWorker = class
  constructor Create(aWorkerId: Integer);  // 5ms initialization
  function DoWork(const aData: RawUtf8): RawUtf8;  // 10ms processing
  destructor Destroy; override;  // 2ms cleanup
end;
```

### 2. Pool Manager (`worker.pas`)

The `TWorkerPool` manages worker lifecycle:

```pascal
TWorkerPool = class
  function BorrowWorker: TExpensiveWorker;
  procedure ReturnWorker(aWorker: TExpensiveWorker);
  procedure GetStats(...);  // Pool statistics
end;
```

**Thread Safety**: Uses `TSynObjectListLocked` from mORMot2 for thread-safe operations.

**Pool Strategy**:
- **Borrow**: Takes from pool if available, creates new if empty
- **Return**: Returns to pool if not full, destroys if pool at capacity
- **Max Size**: Configurable limit (default 10 workers)

### 3. API Service (`api.impl.pas`)

```pascal
TObjectPoolApi = class(TInjectableObjectRest, IObjectPoolApi)
  function ExecuteSimpleOperation(const aData: RawUtf8): TOperationResultDTO;
  function ExecuteParallelOperations(aCount: Integer): RawUtf8;
  function GetPoolStats: TPoolStatsDTO;
end;
```

**Service Registration**: Uses `sicSingle` to ensure one pool instance shared across all requests.

## DMVC → mORMot2 Mapping

| DMVC Concept | mORMot2 Equivalent | Notes |
|--------------|-------------------|-------|
| `TObjectPool<T>` | `TWorkerPool` (custom) | mORMot2 doesn't have built-in generic pool |
| Thread sync | `TSynObjectListLocked` | Thread-safe list with `TRWLock` |
| Controller | Interface-based service | `IObjectPoolApi` interface |
| Middleware | N/A | Not needed for this demo |
| JSON responses | Automatic | mORMot2 handles DTO serialization |

## Usage

### Build and Run

```bash
# Compile
cd /mnt/w/mORMot2/ex/dmvc/55-objectpool
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 55-objectpool.dproj

# Run
./Win32/Debug/ObjectPoolDemo.exe
```

### Test Endpoints

#### 1. Execute Simple Operation

Tests borrowing/returning a single worker:

```bash
curl -X POST http://localhost:8080/ObjectPoolApi/ExecuteSimpleOperation \
  -H "Content-Type: application/json" \
  -d '{"aData":"test data"}'
```

Response:
```json
{
  "Success": true,
  "Message": "Worker #1 processed: \"test data\" (usage count: 1)",
  "ExecutionTimeMs": 15.234,
  "WorkerId": 1
}
```

#### 2. Execute Parallel Operations

Demonstrates pool reuse across multiple operations:

```bash
curl -X POST http://localhost:8080/ObjectPoolApi/ExecuteParallelOperations \
  -H "Content-Type: application/json" \
  -d '{"aCount":50}'
```

Response:
```json
{
  "operations": 50,
  "totalTimeMs": 523,
  "avgTimeMs": 10.46,
  "poolReused": true
}
```

**Without Pooling**: 50 operations × (5ms create + 10ms work + 2ms destroy) = **850ms**
**With Pooling**: 50 operations × 10ms work (reusing 10 workers) = **~520ms** (38% faster!)

#### 3. Get Pool Statistics

```bash
curl http://localhost:8080/ObjectPoolApi/GetPoolStats
```

Response:
```json
{
  "TotalCreated": 10,
  "TotalDestroyed": 0,
  "CurrentActive": 0,
  "CurrentPooled": 10,
  "TotalBorrowed": 50,
  "TotalReturned": 50,
  "PeakActive": 1
}
```

**Statistics Explained**:
- `TotalCreated`: Workers created since server start
- `TotalDestroyed`: Workers destroyed (when pool was full)
- `CurrentActive`: Workers currently in use
- `CurrentPooled`: Workers available in pool
- `TotalBorrowed`: Total borrow operations
- `TotalReturned`: Total return operations
- `PeakActive`: Maximum concurrent active workers

#### 4. Get Pool Info

```bash
curl http://localhost:8080/ObjectPoolApi/GetPoolInfo
```

Response:
```json
"WorkerPool: MaxSize=10, CurrentPooled=10"
```

#### 5. Reset Statistics

```bash
curl -X POST http://localhost:8080/ObjectPoolApi/ResetPoolStats
```

## Performance Analysis

### Benchmark Results

Test scenario: 100 operations

**Without Pooling**:
```
Time = 100 × (5ms create + 10ms work + 2ms destroy)
     = 100 × 17ms
     = 1700ms
Objects created: 100
Objects destroyed: 100
```

**With Pooling (max 10)**:
```
Time = 10 × 5ms create (initial) + 100 × 10ms work
     = 50ms + 1000ms
     = 1050ms
Objects created: 10
Objects destroyed: 0
```

**Improvement**: 38% faster, 90% fewer allocations

### When Pool Size Matters

Pool size should balance:
- **Too small**: Workers get created/destroyed frequently (defeats pooling)
- **Too large**: Memory waste, longer idle times
- **Just right**: Handles typical load with minimal creation

Example with 50 concurrent requests:
- Pool size 5: 45 workers created/destroyed per burst
- Pool size 10: 40 workers created/destroyed per burst
- Pool size 50: 0 workers created/destroyed (optimal for this load)

## Key Differences from DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| Generic Pool | `TObjectPool<T>` generic | Custom `TWorkerPool` class |
| Thread Sync | `TMonitor` | `TSynObjectListLocked` |
| Service Lifecycle | Controller-based | Interface-based service |
| Stats Tracking | Manual | Built into pool implementation |
| JSON Serialization | Manual DTOs | Automatic with packed records |

## Advanced Topics

### Custom Pool for Your Objects

To create a pool for your own objects:

```pascal
type
  TMyExpensiveObject = class
    // Your expensive object
  end;

  TMyObjectPool = class
  private
    fPool: TSynObjectListLocked;
  public
    function Borrow: TMyExpensiveObject;
    procedure Return(aObj: TMyExpensiveObject);
  end;
```

### Pool Strategies

Different pooling strategies for different scenarios:

1. **Fixed Size**: Pre-create all objects (predictable memory)
2. **Dynamic Grow**: Create on demand up to max (this sample's approach)
3. **Time-Based Eviction**: Destroy idle objects after timeout
4. **Priority Queue**: Return most-recently-used first (better cache locality)

### Production Considerations

For production use, consider:

1. **Idle Timeout**: Destroy workers idle > N minutes
2. **Health Checks**: Validate worker state before borrowing
3. **Metrics**: Expose pool stats to monitoring systems
4. **Configuration**: Make pool size configurable
5. **Circuit Breaker**: Stop borrowing if pool exhausted

## See Also

- **46-profiling** - Performance monitoring and timing
- **47-profiling_showcase** - Advanced profiling techniques
- **35-sessions** - Service lifecycle management (sicPerSession)
- **mormot.core.data** - `TSynObjectListLocked` implementation
- **mormot.core.threads** - Thread pool patterns

## Learn More

- [mORMot2 Thread Safety](https://synopse.info/fossil/wiki?name=SQLite3+Framework+Threads)
- [Object Pooling Patterns](https://en.wikipedia.org/wiki/Object_pool_pattern)
- [Performance Optimization Guide](../../docs/performance.md)

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
