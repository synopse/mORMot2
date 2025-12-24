# mORMot2 Data Pump Sample

Port of DelphiMVCFramework's `datapump` sample to mORMot2.

## Overview

This sample demonstrates ETL (Extract, Transform, Load) patterns and bulk data operations using mORMot2's ORM and batch capabilities. It's equivalent to DMVC's ActiveRecord datapump functionality.

## What It Demonstrates

1. **ORM Entity Definitions** (`entities.pas`)
   - Customer and Order entities with relationships
   - Type-safe ORM classes similar to DMVC's `TMVCActiveRecord`

2. **Data Extraction**
   - Reading data from a source SQLite database
   - Using `RetrieveList<T>()` for bulk retrieval
   - Performance-optimized queries

3. **Data Transformation**
   - Mapping foreign key IDs between databases
   - Optional data transformations during transfer
   - Field-level data manipulation

4. **Data Loading**
   - Batch inserts using `TRestBatch`
   - High-performance bulk operations
   - Transaction support for data integrity

5. **Database Operations**
   - Creating database schemas
   - Sample data generation
   - Record counting and verification

## Key mORMot2 Features Used

- **ORM (Object-Relational Mapping)**
  - `TOrmCustomer` and `TOrmOrder` classes
  - Automatic table creation
  - Type-safe queries

- **Batch Operations**
  - `TRestBatch` for efficient bulk inserts
  - Batching 1000 records at a time
  - Significant performance improvement over individual inserts

- **REST Server**
  - `TRestServerDB` for SQLite database access
  - Connection pooling support
  - Thread-safe operations

## DMVC vs mORMot2 Comparison

### DMVC Approach
```pascal
// DMVC uses ActiveRecord pattern
ActiveRecordConnectionsRegistry.SetCurrent('source');
var lCustomers := TMVCActiveRecord.All<TCustomer>;

ActiveRecordConnectionsRegistry.SetCurrent('destination');
for var lCustomer in lCustomers do
begin
  lCustomer.InvalidateConnection;
  lCustomer.Insert;
end;
```

### mORMot2 Approach
```pascal
// mORMot2 uses REST ORM with batch operations
customers := sourceRest.ORM.RetrieveList<TOrmCustomer>('');

batch := TRestBatch.Create(destRest.ORM, TOrmCustomer, 1000);
for customer in customers do
  batch.Add(customer, true);
destRest.ORM.BatchSend(batch);
```

## Running the Sample

1. Compile and run the program
2. The sample will automatically:
   - Create a source database with 100 customers and 500 orders
   - Create an empty destination database
   - Pump all data from source to destination
   - Perform foreign key mapping
   - Demonstrate batch insert of 1000 additional records
   - Display timing and throughput statistics

## Performance Notes

- **Batch Operations**: Uses batches of 1000 records for optimal performance
- **Transaction Support**: All batch operations are wrapped in transactions
- **Throughput**: Can achieve 10,000+ records/second on modern hardware
- **Memory Efficient**: Uses streaming where possible

## File Structure

```
53-datapump/
├── 53-datapump.dpr          # Main program
├── 53-datapump.dproj        # Delphi project file
├── README.md                # This file
└── src/
    ├── entities.pas         # ORM entity definitions
    └── datapump.pas         # Data pump implementation
```

## Output Example

```
mORMot2 Data Pump Sample
========================
Port of DMVC datapump to mORMot2
Demonstrates ETL (Extract, Transform, Load) patterns

Step 1: Creating source database with sample data...
  Generating sample customers...
  Generating sample orders...
  Created 100 customers
  Created 500 orders

Step 2: Creating destination database schema...
  Destination database ready

Step 3: Pumping data from source to destination...
  Opening source database...
  Opening destination database...
  Extracting customers...
    Found 100 customers
  Loading customers to destination (using batch)...
    Successfully inserted 100 customers
  Extracting orders...
    Found 500 orders
  Loading orders to destination (with FK mapping)...
    Successfully inserted 500 orders
  Data pump completed in 123.45ms (123,450μs)
  Total records: 600
  Throughput: 4,858 records/sec

Step 4: Verifying data transfer...
  Source customers: 100
  Dest customers:   100
  Source orders:    500
  Dest orders:      500

Step 5: Demonstrating batch operations...
  Opening destination database...
  Inserting 1000 additional customers using batch...
    Successfully inserted 1000 customers in 45.67ms (45,670μs)
  Throughput: 21,896 records/sec

All operations completed successfully!

Database files:
  Source:      W:\mORMot2\ex\dmvc\53-datapump\Win32\Debug\source.db
  Destination: W:\mORMot2\ex\dmvc\53-datapump\Win32\Debug\destination.db
```

## Related mORMot2 Documentation

- ORM Tutorial: `/doc/mORMot2.pdf` - Chapter on ORM
- Batch Operations: See `mormot.orm.core.pas` - `TRestBatch` class
- SQLite Integration: See `mormot.orm.sqlite3.pas`

## See Also

- **05-datasets**: Data manipulation and JSON serialization
- **06-articles_crud_server**: CRUD operations with business rules
- **07-master_details**: Master-detail relationships
