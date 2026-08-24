
# Task Manager API Documentation

## Overview

This document describes the CQRS SOA API provided by the mORMot2 Task Manager server.

**Base URL**: `http://localhost:8080/taskmanager`

**Content-Type**: All requests and responses use `application/json`

**Request Format**: All endpoints use `POST` with a JSON array of parameters.

**Response Format**: JSON object with a `Result` key containing the return value:
```json
{"Result": <value>}
```

### JSON field casing

DTO field names cross the wire in **camelCase**, not the Pascal `PascalCase` of the
record declarations. Each DTO unit normalizes its wire contract once in its
`initialization` with `Rtti[TypeInfo(T)].Props.NameChangeCase(scLowerCaseFirst)`
(mORMot's "aka camelCase, for API" mode) — applied on the DTO types only, never on
the `TOrm` (see the mORMot2 SAD [*Recommended Patterns*](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md)
[A.7](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a7-copying-between-objects-records-and-dtos), Gotcha 3). The single switch lives in `shared_types.DtoJsonCase`.

`scLowerCaseFirst` lowercases only the first character, so a bare `ID` field becomes
`iD` and `TagIDs` becomes `tagIDs`; multi-word fields read naturally (`priorityName`,
`isCompleted`, `dueDate`). The examples below show the exact field names as they
appear on the wire.

## TaskQuery — Read Operations

### GetTaskView

Returns full details for a single task, including embedded comments and tag IDs.

```
POST /taskmanager/TaskQuery.GetTaskView
Body: [1]
```

**Parameters**: `[aTaskID: TID]`

**Response**:
```json
{
  "Result": {
    "iD": 1,
    "title": "Welcome to Task Manager",
    "description": "This is a sample task",
    "priority": 2,
    "priorityName": "Medium",
    "dueDate": "2026-03-25T10:00:00",
    "status": "pending",
    "isCompleted": false,
    "createdAt": "2026-03-18T10:00:00",
    "updatedAt": "2026-03-18T10:00:00",
    "commentCount": 1,
    "tagIDs": [3],
    "comments": [
      {
        "content": "Started reading the documentation",
        "author": "Developer",
        "createdAt": "2026-03-18T10:00:00",
        "updatedAt": "2026-03-18T10:00:00",
        "isEdited": false
      }
    ]
  }
}
```

### ListTasks

Lists tasks as compact list items, optionally filtered by status.

```
POST /taskmanager/TaskQuery.ListTasks
Body: [""]
```

**Parameters**: `[aStatus: RawUtf8]` — pass `""` for all, or `"pending"`, `"in_progress"`, `"completed"`

**Response**:
```json
{
  "Result": [
    {
      "iD": 1,
      "title": "Welcome to Task Manager",
      "priority": 2,
      "priorityName": "Medium",
      "status": "pending",
      "isCompleted": false,
      "dueDate": "2026-03-25T10:00:00",
      "commentCount": 0,
      "tagCount": 1
    }
  ]
}
```

### SearchTasks

Searches tasks by title/description using FTS5 full-text search (with LIKE fallback).

```
POST /taskmanager/TaskQuery.SearchTasks
Body: [{"searchTerm": "mORMot", "status": ""}]
```

**Parameters**: `[aCriteria: TTaskSearchDTO]`

**Response**: Same format as ListTasks.

## TaskCommand — Write Operations

All command operations return a `TCommandResult`:
```json
{"Result": {"success": true, "iD": 4, "errorMessage": "", "status": 0}}
```

### CreateTask

Creates a new task.

```
POST /taskmanager/TaskCommand.CreateTask
Body: [{"title": "New Task", "description": "Details", "priority": 2, "dueDate": "2026-04-01"}]
```

**Parameters**: `[aData: TTaskCreateDTO]`
- `title` (required): Task title
- `description`: Task description
- `priority` (required): 1-5 (Low to Critical)
- `dueDate`: ISO 8601 date string (optional)

### UpdateTask

Updates an existing task's properties.

```
POST /taskmanager/TaskCommand.UpdateTask
Body: [{"taskID": 1, "title": "Updated", "description": "New desc", "priority": 3, "dueDate": "2026-04-15"}]
```

**Parameters**: `[aData: TTaskUpdateDTO]`

### DeleteTask

Deletes a task and its FTS5 index entry.

```
POST /taskmanager/TaskCommand.DeleteTask
Body: [1]
```

**Parameters**: `[aTaskID: TID]`

### MarkComplete

Toggles task completion status.

```
POST /taskmanager/TaskCommand.MarkComplete
Body: [1, true]
```

**Parameters**: `[aTaskID: TID, aIsComplete: boolean]`

### AddComment

Adds an embedded comment to a task.

```
POST /taskmanager/TaskCommand.AddComment
Body: [{"taskID": 1, "content": "Great progress!", "author": "John"}]
```

**Parameters**: `[aData: TTaskAddCommentDTO]`
- `taskID` (required): Target task ID
- `content` (required): Comment text (1-10000 chars)
- `author` (required): Author name

### UpdateComment

Updates an existing comment by its array index.

```
POST /taskmanager/TaskCommand.UpdateComment
Body: [{"taskID": 1, "commentIndex": 0, "content": "Updated text"}]
```

**Parameters**: `[aData: TTaskUpdateCommentDTO]`

### DeleteComment

Deletes a comment by its array index.

```
POST /taskmanager/TaskCommand.DeleteComment
Body: [1, 0]
```

**Parameters**: `[aTaskID: TID, aCommentIndex: integer]`

### AddTag

Associates a tag with a task (idempotent).

```
POST /taskmanager/TaskCommand.AddTag
Body: [1, 2]
```

**Parameters**: `[aTaskID: TID, aTagID: TID]`

### RemoveTag

Removes a tag association from a task.

```
POST /taskmanager/TaskCommand.RemoveTag
Body: [1, 2]
```

**Parameters**: `[aTaskID: TID, aTagID: TID]`

## TagQuery — Read Operations

### GetTagView

Returns details for a single tag.

```
POST /taskmanager/TagQuery.GetTagView
Body: [1]
```

**Parameters**: `[aTagID: TID]`

**Response**:
```json
{
  "Result": {
    "iD": 1,
    "name": "urgent",
    "color": "#FF3333",
    "createdAt": "2026-03-18T10:00:00"
  }
}
```

### ListTags

Returns all tags.

```
POST /taskmanager/TagQuery.ListTags
Body: []
```

**Response**:
```json
{
  "Result": [
    {"iD": 1, "name": "urgent", "color": "#FF3333", "createdAt": "2026-03-18T10:00:00"},
    {"iD": 2, "name": "work", "color": "#3498DB", "createdAt": "2026-03-18T10:00:00"}
  ]
}
```

### SearchTags

Searches tags by name (case-insensitive partial match).

```
POST /taskmanager/TagQuery.SearchTags
Body: ["urg"]
```

**Parameters**: `[aSearchTerm: RawUtf8]`

## TagCommand — Write Operations

All command operations return `TCommandResult`.

### CreateTag

Creates a new tag (duplicate names rejected).

```
POST /taskmanager/TagCommand.CreateTag
Body: [{"name": "feature", "color": "#00FF00"}]
```

**Parameters**: `[aData: TTagCreateDTO]`

### UpdateTag

Updates a tag's name and/or color.

```
POST /taskmanager/TagCommand.UpdateTag
Body: [{"tagID": 1, "name": "critical", "color": "#FF0000"}]
```

**Parameters**: `[aData: TTagUpdateDTO]`

### DeleteTag

Deletes a tag.

```
POST /taskmanager/TagCommand.DeleteTag
Body: [1]
```

**Parameters**: `[aTagID: TID]`

## DTO Reference

The **Field** column shows the JSON name as it appears on the wire (camelCase); the
Pascal record member is the same name in `PascalCase`.

### TTaskCreateDTO
| Field | Type | Description |
|-------|------|-------------|
| title | RawUtf8 | Task title (required) |
| description | RawUtf8 | Task description |
| priority | integer | 1=Low, 2=Medium, 3=High, 4=Urgent, 5=Critical |
| dueDate | RawUtf8 | ISO 8601 date string |

### TTaskUpdateDTO
| Field | Type | Description |
|-------|------|-------------|
| taskID | TID | Task to update |
| title | RawUtf8 | New title |
| description | RawUtf8 | New description |
| priority | integer | New priority (1-5) |
| dueDate | RawUtf8 | New due date |

### TTaskAddCommentDTO
| Field | Type | Description |
|-------|------|-------------|
| taskID | TID | Target task |
| content | RawUtf8 | Comment text (1-10000 chars) |
| author | RawUtf8 | Author name |

### TTaskUpdateCommentDTO
| Field | Type | Description |
|-------|------|-------------|
| taskID | TID | Target task |
| commentIndex | integer | Zero-based comment index |
| content | RawUtf8 | New comment text |

### TTagCreateDTO
| Field | Type | Description |
|-------|------|-------------|
| name | RawUtf8 | Tag name (unique) |
| color | RawUtf8 | Color code (#RRGGBB) |
| reserved | RawUtf8 | Unused padding; clients may omit it. Present only to work around a mORMot SOA quirk where a 2-field record parameter deserializes to empty fields. |

### TTagUpdateDTO
| Field | Type | Description |
|-------|------|-------------|
| tagID | TID | Tag to update |
| name | RawUtf8 | New name |
| color | RawUtf8 | New color |

### TCommandResult
| Field | Type | Description |
|-------|------|-------------|
| success | boolean | Whether the operation succeeded |
| iD | TID | Affected entity ID |
| errorMessage | RawUtf8 | Error description (empty on success) |
| status | TServiceResult | Result discriminator (enum ordinal) |

### TTaskViewDTO
| Field | Type | Description |
|-------|------|-------------|
| iD | TID | Task ID |
| title | RawUtf8 | Task title |
| description | RawUtf8 | Task description |
| priority | integer | Priority level (1-5) |
| priorityName | RawUtf8 | Human-readable priority |
| dueDate | TDateTime | Due date |
| status | RawUtf8 | Task status |
| isCompleted | boolean | Completion flag |
| createdAt | TDateTime | Creation timestamp |
| updatedAt | TDateTime | Last update timestamp |
| commentCount | integer | Number of comments |
| tagIDs | TIDDynArray | Associated tag IDs |
| comments | TTaskCommentDynArray | Embedded comments |

### TTaskListItemDTO
| Field | Type | Description |
|-------|------|-------------|
| iD | TID | Task ID |
| title | RawUtf8 | Task title |
| priority | integer | Priority level (1-5) |
| priorityName | RawUtf8 | Human-readable priority |
| status | RawUtf8 | Task status |
| isCompleted | boolean | Completion flag |
| dueDate | TDateTime | Due date |
| commentCount | integer | Number of comments |
| tagCount | integer | Number of tags |

### TTagViewDTO
| Field | Type | Description |
|-------|------|-------------|
| iD | TID | Tag ID |
| name | RawUtf8 | Tag name |
| color | RawUtf8 | Color code |
| createdAt | TDateTime | Creation timestamp |

## Examples

### JavaScript/Fetch API

```javascript
const API_BASE = '/taskmanager';

// SOA call helper
async function soaCall(service, method, params) {
    const response = await fetch(`${API_BASE}/${service}.${method}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(params)
    });
    return response.json();
}

// List all tasks
const data = await soaCall('TaskQuery', 'ListTasks', ['']);
const tasks = data.Result;

// Create task (request DTO fields are camelCase too)
const result = await soaCall('TaskCommand', 'CreateTask', [{
    title: 'New Task',
    description: 'Description',
    priority: 2,
    dueDate: '2026-04-01'
}]);
if (result.Result.success) {
    console.log('Created task ID:', result.Result.iD);
}

// Add comment
await soaCall('TaskCommand', 'AddComment', [{
    taskID: 1,
    content: 'Great progress!',
    author: 'Developer'
}]);
```

### cURL

```bash
# List tasks
curl -s -X POST http://localhost:8080/taskmanager/TaskQuery.ListTasks \
  -H "Content-Type: application/json" -d '[""]'

# Create task
curl -s -X POST http://localhost:8080/taskmanager/TaskCommand.CreateTask \
  -H "Content-Type: application/json" \
  -d '[{"title":"New Task","description":"Test","priority":2,"dueDate":"2026-04-01"}]'

# Get task details
curl -s -X POST http://localhost:8080/taskmanager/TaskQuery.GetTaskView \
  -H "Content-Type: application/json" -d '[1]'

# Delete task
curl -s -X POST http://localhost:8080/taskmanager/TaskCommand.DeleteTask \
  -H "Content-Type: application/json" -d '[1]'

# List tags
curl -s -X POST http://localhost:8080/taskmanager/TagQuery.ListTags \
  -H "Content-Type: application/json" -d '[]'
```

## Statistics

The framework's built-in `stat` method-based service reports per-method statistics for every CQRS service — call counts, timing, input/output sizes and error counts — collected automatically, with no instrumentation code in the services:

```bash
curl -s "http://localhost:8080/taskmanager/stat?withall=1"
```

The same counters are aggregated per hour/day/month/year into the `MonitorUsage` table of the SQLite database (see `TSynMonitorUsageRest` in `serv/app/ServAppTaskManager.pas`).

## CORS

The server is configured with CORS enabled (`Access-Control-Allow-Origin: *`).

## Notes

- All datetime fields use ISO 8601 format in responses
- The server automatically creates database tables on first run
- Sample data is created if the database is empty
- All DTOs are `packed record` types registered with `Rtti.RegisterFromText` for JSON serialization
- DTO field names are normalized to camelCase on the wire via
  `NameChangeCase(scLowerCaseFirst)` — on the DTOs only, never the `TOrm`
  (*Recommended Patterns* [A.7](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a7-copying-between-objects-records-and-dtos), Gotcha 3)
