# 30-simple_api_using_mvcactiverecord - Simple Customer API

**Port of**: DMVCFramework `samples/simple_api_using_mvcactiverecord`
**Difficulty**: Low
**Demonstrates**: Simple ORM-based API, transaction support, bulk operations

## Overview

A streamlined RESTful API for Customer management using mORMot2's ORM. This is the simplest possible ActiveRecord-style API, perfect for getting started.

## Key Features

- Single entity (Customer) CRUD operations
- Filtering by city
- Bulk create with transaction support
- Automatic JSON serialization
- SQLite persistence

## Building & Running

```bash
# Compile
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 30-simple_api_using_mvcactiverecord.dproj

# Run
30-simple_api_using_mvcactiverecord.exe
```

## API Endpoints

```bash
# Get all customers
curl http://localhost:8080/customers/CustomerApi/GetAllCustomers

# Get customer by ID
curl http://localhost:8080/customers/CustomerApi/GetCustomer?id=1

# Get customers by city
curl http://localhost:8080/customers/CustomerApi/GetCustomersByCity?city=Gotham

# Create customer
curl -X POST http://localhost:8080/customers/CustomerApi/CreateCustomer \
  -H "Content-Type: application/json" \
  -d '{"code":"TEST","companyName":"Test Corp","city":"Boston","rating":4,"note":"New customer"}'

# Bulk create (transaction)
curl -X POST http://localhost:8080/customers/CustomerApi/BulkCreateCustomers \
  -H "Content-Type: application/json" \
  -d '[{"code":"A","companyName":"A Corp","city":"NY","rating":5,"note":""},
       {"code":"B","companyName":"B Corp","city":"LA","rating":4,"note":""}]'
```

## DMVC → mORMot2 Mapping

| DMVC | mORMot2 |
|------|---------|
| `TCustomer.Insert` | `Server.Orm.Add(customer)` |
| `TMVCActiveRecord.GetByPK<T>` | `T.Create(Server.Orm, id)` |
| `TMVCActiveRecord.SelectRQL<T>` | `Server.Orm.RetrieveListObjArray` |
| `CurrentConnection.StartTransaction` | `Server.Orm.TransactionBegin` |
| `CurrentConnection.Commit` | `Server.Orm.Commit` |

## See Also

- **Sample 28** - Full ActiveRecord CRUD with multiple entities
- **Sample 31** - Dataset-based API (alternative approach)

---

**Created**: 2025-12-20
