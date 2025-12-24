# mORMot2 DMVC Port - Sample 56: Articles CRUD VCL Client

## Purpose

VCL desktop client application that demonstrates consuming the mORMot2 REST API from the `06-articles_crud_server` sample. This shows how to build a complete CRUD application with a rich client interface.

## Features

### Core Functionality
- **Full CRUD Operations**: Create, Read, Update, Delete articles via REST API
- **Interface-based Client**: Uses mORMot2's interface-based REST client (`IArticlesApi`)
- **Grid Display**: Shows all articles in a `TStringGrid` with sortable columns
- **Detail Editing**: Modal form for creating and editing articles
- **Search**: Search articles by description with live results
- **Automatic Refresh**: Grid updates after all operations

### UI Components
- **Main Form**: Article list with toolbar buttons
- **Edit Form**: Dedicated form for article create/edit operations
- **Search Panel**: Collapsible search panel with text input
- **Status Bar**: Real-time operation status and error messages

### Business Rules Implementation
The client validates business rules locally before sending to server:
- **Code Format**: Must be CXX, CXXX, or CXXXX (e.g., C01, C123, C1234)
- **Price Validation**:
  - Must be > 0 for new articles
  - Must be > 2 for updates
  - Server enforces additional rule (> 5 for deletes)
- **Field Requirements**: Code and Description are mandatory

### mORMot2 Features Demonstrated
- Interface-based REST client (`TRestHttpClientGeneric`)
- Service resolution (`Services.Resolve`)
- DTO record serialization/deserialization
- JSON-RPC style method calls
- Exception handling in REST context
- Currency type handling over JSON

## Prerequisites

1. **Running Server**: Must have `06-articles_crud_server` running on `localhost:8080`
2. **mORMot2**: Requires mORMot2 source in `../../..` relative to sample directory

## Usage

### Starting the Client

1. First start the server:
   ```bash
   cd ../06-articles_crud_server
   ./06-articles_crud_server.exe
   ```

2. Then run the client:
   ```bash
   ./56-articles_crud_vcl_client.exe
   ```

3. The client connects automatically on startup and loads articles

### Operations

#### View All Articles
- Click **Refresh** button to reload all articles from server
- Grid shows: ID, Code, Description, Price, Created At, Updated At
- Double-click any row to edit

#### Create New Article
1. Click **New** button
2. Enter article details:
   - Code: Format CXX-CXXXX (e.g., C123)
   - Description: Any text (max 200 chars)
   - Price: Decimal number > 0
3. Click **OK** to save

#### Edit Article
1. Select row in grid or double-click
2. Click **Edit** button
3. Modify details (Price must be > 2)
4. Click **OK** to save

#### Delete Article
1. Select row in grid
2. Click **Delete** button
3. Confirm deletion
4. Note: Server rejects deletion if Price ≤ 5

#### Search Articles
1. Click **Search** button to show search panel
2. Enter search term (searches in Description field)
3. Click **Search** button in panel
4. Click **Clear** to return to full list

## API Integration

### Interface Resolution
```pascal
fClient := TRestHttpClientGeneric.Create('localhost', '8080', TOrmModel.Create([]));
fClient.ServiceDefine([IArticlesApi], sicClientDriven);
if not fClient.Services.Resolve(IArticlesApi, fApi) then
  raise Exception.Create('Failed to resolve IArticlesApi interface');
```

### API Methods Used
- `GetAll(): TArticleDtos` - Load all articles
- `Search(query: RawUtf8): TArticleDtos` - Search by description
- `GetById(id: TID): TArticleDto` - Get single article (used internally)
- `Create(article: TArticleDto): TID` - Create new article
- `Update(id: TID; article: TArticleDto)` - Update existing article
- `Delete(id: TID)` - Delete article

### Error Handling
All API calls are wrapped in try-except blocks that:
- Display error messages in status label
- Show modal error dialog for user awareness
- Maintain UI state (e.g., don't close edit form on validation failure)
- Automatically refresh grid on successful operations

## Architecture

### Forms
- **TMainForm** (`MainFormU.pas`): Main application window
  - Manages REST client connection
  - Displays article grid
  - Handles toolbar operations
  - Coordinates with edit form

- **TArticleEditForm** (`ArticleEditFormU.pas`): Article editor
  - Modal dialog for create/edit
  - Client-side validation
  - Supports both new and edit modes
  - Returns validated `TArticleDto`

### Data Flow
```
User Action → UI Event Handler → API Call → Server Processing
                ↓                    ↓
          Validation          Error/Success
                ↓                    ↓
          Edit Form          Status Update → Grid Refresh
```

## Code Highlights

### DTO Handling
```pascal
type
  TArticleDto = packed record
    ID: TID;
    Code: RawUtf8;
    Description: RawUtf8;
    Price: Currency;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
  end;
```

### Grid Display
- Uses `TStringGrid` for lightweight, fast display
- Fixed header row with column titles
- Row selection for edit/delete
- Automatic sizing based on data count
- Custom column widths for optimal display

### Validation
- Code format regex: `^C\d{2,4}$`
- Price range checks before API call
- Mandatory field validation
- Real-time feedback via validation label

## Differences from Original DMVC Sample

| Aspect | Original DMVC | mORMot2 Port |
|--------|---------------|--------------|
| **Client Type** | REST client with manual JSON | Interface-based client |
| **API Calls** | HTTP verbs (GET/POST/PUT/DELETE) | Interface methods |
| **JSON Handling** | Manual ObjectToJSON/JSONToObject | Automatic DTO serialization |
| **Error Handling** | HTTP status codes | Exception handling |
| **URL Construction** | Manual path building | Method parameters |
| **Grid Component** | DBGrid with ClientDataSet | TStringGrid with array |

## Building

### Command Line
```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 56-articles_crud_vcl_client.dproj
```

### IDE
1. Open `56-articles_crud_vcl_client.dproj` in RAD Studio
2. Build → Build 56-articles_crud_vcl_client

## Testing Checklist

- [ ] Client connects to server on startup
- [ ] All articles load and display correctly
- [ ] New article creation with valid data succeeds
- [ ] Code format validation rejects invalid formats
- [ ] Price validation rejects values ≤ 0 (new) or ≤ 2 (edit)
- [ ] Edit operation updates article on server
- [ ] Delete operation removes article (if price > 5)
- [ ] Delete operation fails with error (if price ≤ 5)
- [ ] Search finds matching articles
- [ ] Clear search returns to full list
- [ ] Double-click grid row opens edit form
- [ ] Status messages display correctly
- [ ] Error messages show in dialog and status
- [ ] Grid refreshes after all operations

## Notes

- **Thread Safety**: All API calls are on main thread (VCL requirement)
- **Connection Management**: Single persistent connection throughout session
- **Memory Management**: Interface reference counting handles cleanup
- **UTF-8 Handling**: All strings converted between UTF-8 (API) and String (UI)
- **Currency Precision**: Server maintains full currency precision
- **Timestamps**: Server manages CreatedAt/UpdatedAt automatically

## Related Samples

- **06-articles_crud_server**: The server this client connects to
- **19-basicdemo_vclclient**: Simpler VCL client example
- **21-restclient**: Lower-level REST client techniques

## Ported From

DelphiMVCFramework `articles_crud_vcl_client` sample
Original: https://github.com/danieleteti/delphimvcframework
Port Date: December 2024
