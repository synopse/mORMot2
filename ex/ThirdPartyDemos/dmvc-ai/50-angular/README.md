# 50-angular - Angular/SPA Integration Sample

Port of DMVC `angular` sample to mORMot2, demonstrating REST API backend for Angular (and other SPA frameworks).

## Overview

This sample demonstrates how mORMot2 can serve as a powerful REST API backend for modern frontend frameworks like Angular, React, Vue, etc. It includes:

- **Backend**: mORMot2 JSON-RPC REST server with CORS support
- **Frontend**: Simple Vanilla JavaScript SPA (demonstrates the same patterns Angular would use)
- **CRUD Operations**: Full Create, Read, Update, Delete operations
- **Search**: Customer search functionality
- **Sample Data**: Auto-seeded database with sample customers

## Key Features

### Backend (mORMot2)

1. **JSON-RPC API**: Clean interface-based service definitions
2. **CORS Support**: Configured for cross-origin requests from frontend
3. **SQLite Database**: Lightweight ORM with `TOrmCustomer` entity
4. **Validation**: Email validation using mORMot2's built-in validators
5. **Logging**: Comprehensive logging with mORMot2's TSynLog

### Frontend (Vanilla JS)

1. **Modern UI**: Responsive design with gradients and smooth animations
2. **CRUD Interface**: Add, edit, delete customers with modal dialogs
3. **Search**: Real-time customer search
4. **Error Handling**: User-friendly error messages
5. **No Framework**: Pure JavaScript to show REST API patterns clearly

## Architecture

```
┌─────────────────────┐         HTTP/JSON-RPC          ┌──────────────────────┐
│   Frontend SPA      │  ←─────────────────────────→  │   mORMot2 Backend    │
│   (Vanilla JS)      │         CORS Enabled          │   (Delphi)           │
│                     │                                │                      │
│  - index.html       │                                │  - TAngularServer    │
│  - app.js           │                                │  - ICustomerApi      │
│  - Customer CRUD UI │                                │  - TOrmCustomer      │
└─────────────────────┘                                └──────────────────────┘
                                                                   │
                                                                   ▼
                                                        ┌──────────────────────┐
                                                        │  SQLite Database     │
                                                        │  - Customers table   │
                                                        └──────────────────────┘
```

## API Endpoints

All endpoints use JSON-RPC style POST requests to `http://localhost:8080/CustomerApi.[Method]`

### GetAll
```http
POST http://localhost:8080/CustomerApi.GetAll
```
Returns: Array of all customers

### GetById
```http
POST http://localhost:8080/CustomerApi.GetById
Content-Type: application/json

{"id": 1}
```
Returns: Single customer object

### Search
```http
POST http://localhost:8080/CustomerApi.Search
Content-Type: application/json

{"query": "John"}
```
Returns: Array of matching customers (searches FirstName, LastName, Email)

### CreateCustomer
```http
POST http://localhost:8080/CustomerApi.CreateCustomer
Content-Type: application/json

{
  "customer": {
    "FirstName": "John",
    "LastName": "Doe",
    "Email": "john.doe@example.com",
    "Phone": "+1-555-1234",
    "City": "New York",
    "Country": "USA"
  }
}
```
Returns: New customer ID

### Update
```http
POST http://localhost:8080/CustomerApi.Update
Content-Type: application/json

{
  "id": 1,
  "customer": {
    "FirstName": "John",
    "LastName": "Smith",
    "Email": "john.smith@example.com",
    "Phone": "+1-555-5678",
    "City": "Boston",
    "Country": "USA"
  }
}
```
Returns: None (void)

### Delete
```http
POST http://localhost:8080/CustomerApi.Delete
Content-Type: application/json

{"id": 1}
```
Returns: None (void)

## Running the Sample

### 1. Start Backend Server

```bash
cd /mnt/w/mORMot2/ex/dmvc/50-angular
./50-angular
```

The server will:
- Start on `http://localhost:8080`
- Create SQLite database: `angular_customers.db`
- Seed database with 5 sample customers
- Enable CORS for frontend requests

### 2. Open Frontend

Simply open `frontend/index.html` in your web browser:

```bash
# Using Python's built-in server (optional)
cd frontend
python3 -m http.server 8000

# Then open: http://localhost:8000
```

Or just open the file directly:
```bash
firefox frontend/index.html
# or
chromium frontend/index.html
```

## CORS Configuration

The backend includes CORS middleware that allows:
- **Origins**: `*` (all origins - adjust for production)
- **Methods**: GET, POST, PUT, DELETE, OPTIONS
- **Headers**: Content-Type, Authorization, X-Requested-With
- **Max Age**: 24 hours (86400 seconds)

### Production CORS Setup

For production, modify `server.pas` to restrict allowed origins:

```pascal
// Replace in OnBeforeBody:
corsHeaders := 'Access-Control-Allow-Origin: https://yourdomain.com'#13#10 + ...
```

## Sample Data

The database is automatically seeded with these customers:

1. John Doe (New York, USA)
2. Jane Smith (Los Angeles, USA)
3. Bob Johnson (Chicago, USA)
4. Alice Williams (London, UK)
5. Charlie Brown (Paris, France)

## Adapting for Real Angular

This sample uses Vanilla JavaScript for simplicity. For a real Angular app:

### 1. Create Angular Service

```typescript
// customer.service.ts
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

@Injectable({ providedIn: 'root' })
export class CustomerService {
  private apiUrl = 'http://localhost:8080/CustomerApi';

  constructor(private http: HttpClient) {}

  getAll(): Observable<Customer[]> {
    return this.http.post<any>(`${this.apiUrl}.GetAll`, {})
      .pipe(map(response => response.result));
  }

  getById(id: number): Observable<Customer> {
    return this.http.post<any>(`${this.apiUrl}.GetById`, { id })
      .pipe(map(response => response.result));
  }

  createCustomer(customer: Customer): Observable<number> {
    return this.http.post<any>(`${this.apiUrl}.CreateCustomer`, { customer })
      .pipe(map(response => response.result));
  }

  update(id: number, customer: Customer): Observable<void> {
    return this.http.post<any>(`${this.apiUrl}.Update`, { id, customer })
      .pipe(map(() => undefined));
  }

  delete(id: number): Observable<void> {
    return this.http.post<any>(`${this.apiUrl}.Delete`, { id })
      .pipe(map(() => undefined));
  }

  search(query: string): Observable<Customer[]> {
    return this.http.post<any>(`${this.apiUrl}.Search`, { query })
      .pipe(map(response => response.result));
  }
}
```

### 2. Customer Model

```typescript
// customer.model.ts
export interface Customer {
  ID: number;
  FirstName: string;
  LastName: string;
  Email: string;
  Phone?: string;
  City?: string;
  Country?: string;
}
```

### 3. Component Example

```typescript
// customer-list.component.ts
import { Component, OnInit } from '@angular/core';
import { CustomerService } from './customer.service';

@Component({
  selector: 'app-customer-list',
  templateUrl: './customer-list.component.html'
})
export class CustomerListComponent implements OnInit {
  customers: Customer[] = [];

  constructor(private customerService: CustomerService) {}

  ngOnInit() {
    this.loadCustomers();
  }

  loadCustomers() {
    this.customerService.getAll().subscribe(
      customers => this.customers = customers,
      error => console.error('Failed to load customers', error)
    );
  }

  deleteCustomer(id: number) {
    if (confirm('Are you sure?')) {
      this.customerService.delete(id).subscribe(
        () => this.loadCustomers(),
        error => console.error('Failed to delete customer', error)
      );
    }
  }
}
```

## Key Differences from DMVC

### 1. API Style
- **DMVC**: RESTful URLs (`GET /api/customers`, `POST /api/customers`)
- **mORMot2**: JSON-RPC (`POST /CustomerApi.GetAll`, `POST /CustomerApi.Create`)

### 2. CORS Implementation
- **DMVC**: `TMVCCORSMiddleware`
- **mORMot2**: Custom `OnBeforeBody` handler

### 3. ORM
- **DMVC**: Active Record pattern (`TMVCActiveRecord`)
- **mORMot2**: Interface-based services with ORM

### 4. Validation
- **DMVC**: Attributes on entity properties
- **mORMot2**: `AddFilterOrValidate` in initialization

## Testing with curl

```bash
# Get all customers
curl -X POST http://localhost:8080/CustomerApi.GetAll

# Get customer by ID
curl -X POST http://localhost:8080/CustomerApi.GetById \
  -H "Content-Type: application/json" \
  -d '{"id":1}'

# Search customers
curl -X POST http://localhost:8080/CustomerApi.Search \
  -H "Content-Type: application/json" \
  -d '{"query":"John"}'

# Create customer
curl -X POST http://localhost:8080/CustomerApi.CreateCustomer \
  -H "Content-Type: application/json" \
  -d '{"customer":{"FirstName":"Test","LastName":"User","Email":"test@example.com"}}'

# Update customer
curl -X POST http://localhost:8080/CustomerApi.Update \
  -H "Content-Type: application/json" \
  -d '{"id":1,"customer":{"FirstName":"Updated","LastName":"Name","Email":"updated@example.com"}}'

# Delete customer
curl -X POST http://localhost:8080/CustomerApi.Delete \
  -H "Content-Type: application/json" \
  -d '{"id":6}'
```

## Files

- `50-angular.dpr` - Main program
- `50-angular.dproj` - Delphi project file
- `src/server.pas` - Server with CORS configuration
- `src/entities.pas` - TOrmCustomer entity
- `src/api.interfaces.pas` - ICustomerApi interface
- `src/api.impl.pas` - TCustomerApi implementation
- `frontend/index.html` - Frontend UI
- `frontend/app.js` - Frontend logic

## See Also

- **06-articles_crud_server** - Another CRUD example
- **13-middleware_cors** - CORS middleware patterns
- **CONVERSION-GUIDE.md** - General DMVC to mORMot2 patterns
