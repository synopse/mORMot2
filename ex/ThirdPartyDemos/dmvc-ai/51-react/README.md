# mORMot2 React Sample

Port of DelphiMVCFramework React sample to mORMot2.

## Overview

This sample demonstrates mORMot2 REST API integration with a React frontend, implementing a complete CRUD application for customer management.

## Architecture

### Backend (Delphi/mORMot2)
- **RESTful API** using mORMot2 HTTP server
- **Interface-based services** (ICustomersApi)
- **ORM** with SQLite database
- **CORS support** for cross-origin requests
- **Validation** at service layer

### Frontend (React)
- **React 16.13** with functional components and hooks
- **React Router 6** for navigation
- **Axios** for HTTP requests
- **Bootstrap 4** for styling

## Running the Sample

### 1. Start the Backend Server

Compile and run the Delphi project:

```bash
# Compile
dcc32 51-react.dpr

# Or compile with delphi-compiler.exe
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 51-react.dproj

# Run
51-react.exe
```

The server will start on `http://localhost:8080`

### 2. Start the React Frontend

```bash
cd frontend
npm install
npm start
```

The React app will start on `http://localhost:3000` and automatically open in your browser.

## API Endpoints

All endpoints use JSON format and support CORS.

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | /api/customers | Get all customers |
| GET | /api/customers/:id | Get customer by ID |
| POST | /api/customers | Create new customer |
| PUT | /api/customers/:id | Update customer |
| DELETE | /api/customers/:id | Delete customer |

## Customer Entity

```json
{
  "ID": 1,
  "Code": "C001",
  "Description": "Acme Corporation",
  "City": "New York",
  "Note": "Main customer",
  "Rating": 5
}
```

### Validation Rules

- **Code**: Required, max length 15
- **Description**: Required, max length 50
- **City**: Optional, max length 50
- **Note**: Optional, max length 255
- **Rating**: 0-5

## Key Features

### Backend Features
- ✅ RESTful routing with mORMot2
- ✅ Interface-based service architecture
- ✅ SQLite database with ORM
- ✅ CORS middleware
- ✅ Validation and error handling
- ✅ Logging with TSynLog
- ✅ Sample data initialization

### Frontend Features
- ✅ Customer list with view/edit/delete
- ✅ Customer creation form
- ✅ Customer editing form
- ✅ Form validation
- ✅ Error display
- ✅ React Router navigation
- ✅ Responsive Bootstrap UI

## Project Structure

```
51-react/
├── 51-react.dpr              # Main program
├── 51-react.dproj            # Delphi project file
├── src/
│   ├── entities.pas          # ORM entities and DTOs
│   ├── api.interfaces.pas    # Service interfaces
│   ├── api.impl.pas          # Service implementation
│   └── server.pas            # HTTP server and routing
└── frontend/
    ├── package.json
    ├── public/
    │   └── index.html
    └── src/
        ├── App.js
        ├── Layout.js
        ├── services/
        │   └── api.js        # Axios configuration
        └── components/
            ├── customers/    # Customer list
            ├── customer/     # Customer form
            └── about/        # About page
```

## Conversion Notes

### DMVC → mORMot2 Mapping

| DMVC | mORMot2 |
|------|---------|
| TWebModule | TReactServer |
| TMVCEngine | TRestHttpServer |
| TMVCController | Interface-based service |
| TMVCActiveRecord | TOrmCustomer + IRestOrm |
| TMVCCORSMiddleware | AccessControlAllowOrigin |
| MVCPath attributes | Route.Get/Post/Put/Delete |
| Context.Request.BodyAs | RecordLoadJson |
| Render() | RecordSaveJson |

### Key Differences

1. **Routing**: mORMot2 uses explicit route registration vs DMVC's attribute-based routing
2. **Services**: mORMot2 uses interface-based services vs DMVC's controller classes
3. **ORM**: mORMot2 ORM vs DMVC ActiveRecord
4. **JSON**: mORMot2 uses RTTI-based JSON vs DMVC's serialization

## Testing

You can test the API directly with curl:

```bash
# Get all customers
curl http://localhost:8080/api/customers

# Get customer by ID
curl http://localhost:8080/api/customers/1

# Create customer
curl -X POST http://localhost:8080/api/customers \
  -H "Content-Type: application/json" \
  -d '{"Code":"C999","Description":"Test Customer","City":"Boston","Rating":3}'

# Update customer
curl -X PUT http://localhost:8080/api/customers/1 \
  -H "Content-Type: application/json" \
  -d '{"Code":"C001","Description":"Updated Name","City":"New York","Rating":5}'

# Delete customer
curl -X DELETE http://localhost:8080/api/customers/1
```

## Requirements

### Backend
- Delphi 12 or later
- mORMot2 framework

### Frontend
- Node.js 14 or later
- npm or yarn

## License

This sample is part of the mORMot2 project and follows the same license.

## See Also

- [mORMot2 Documentation](https://github.com/synopse/mORMot2)
- [Original DMVC React Sample](https://github.com/danieleteti/delphimvcframework/tree/master/samples/react)
- [React Documentation](https://reactjs.org/)
