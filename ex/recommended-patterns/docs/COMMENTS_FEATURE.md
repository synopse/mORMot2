
# Task Comments Feature

## Overview

Comments are **embedded within the Task aggregate** as a JSON array, following DDD principles. There is no separate Comment table — comments are stored as `TTaskCommentDynArray` in a TEXT column of the Task table.

## Architecture

### Embedded Comment Structure

```pascal
TTaskComment = packed record
  Content: RawUtf8;     // Comment text (1-10000 characters)
  Author: RawUtf8;      // Author name
  CreatedAt: TDateTime;  // When created (UTC)
  UpdatedAt: TDateTime;  // When last edited (UTC)
  IsEdited: boolean;     // Whether the comment has been edited
end;
TTaskCommentDynArray = array of TTaskComment;
```

Comments are part of the **TTask aggregate root**:

```pascal
TTask = class(TOrm)
published
  property Comments: TTaskCommentDynArray read fComments write fComments;
  // ... other fields
end;
```

In the database, the `Comments` column contains a JSON array:
```json
[
  {"content":"Started reading the docs","author":"Developer","createdAt":"2026-03-18T10:00:00","updatedAt":"2026-03-18T10:00:00","isEdited":false},
  {"content":"Made good progress","author":"Developer","createdAt":"2026-03-18T14:00:00","updatedAt":"2026-03-18T14:00:00","isEdited":false}
]
```

### Why Embedded?

- **Consistency**: Comments and their parent task are always in sync (single write)
- **No joins**: Reading a task with all its comments is a single row fetch
- **Aggregate boundary**: Comments don't exist independently — they belong to a task
- **Simplicity**: No junction tables, no foreign keys, no orphan cleanup

### Trade-offs

- Comments are accessed by array index (not by unique ID)
- The entire comment array is read/written on every modification
- Not suitable for tasks with thousands of comments (practical for typical use)

## CQRS Interface

Comment operations are part of the **ITaskCommand** interface (write side):

```pascal
ITaskCommand = interface(IInvokable)
  function AddComment(const aData: TTaskAddCommentDTO): TCommandResult;
  function UpdateComment(const aData: TTaskUpdateCommentDTO): TCommandResult;
  function DeleteComment(aTaskID: TID; aCommentIndex: integer): TCommandResult;
  // ... other task commands
end;
```

Comments are returned in the **ITaskQuery** response:

```pascal
ITaskQuery = interface(IInvokable)
  function GetTaskView(aTaskID: TID): TTaskViewDTO;
  // TTaskViewDTO includes CommentCount, TagIDs, and Comments array
end;
```

## DTOs

### TTaskAddCommentDTO (Write)

```pascal
TTaskAddCommentDTO = packed record
  TaskID: TID;       // Target task
  Content: RawUtf8;  // Comment text
  Author: RawUtf8;   // Author name
end;
```

### TTaskUpdateCommentDTO (Write)

```pascal
TTaskUpdateCommentDTO = packed record
  TaskID: TID;          // Target task
  CommentIndex: integer; // Zero-based index in Comments array
  Content: RawUtf8;     // New content
end;
```

### TTaskViewDTO (Read)

Contains embedded comments:
```pascal
TTaskViewDTO = packed record
  // ... task fields
  CommentCount: integer;
  Comments: TTaskCommentDynArray;  // Full comment data
end;
```

## API Endpoints

### Add Comment

```
POST /taskmanager/TaskCommand.AddComment
Body: [{"taskID": 1, "content": "Great progress!", "author": "John"}]
Response: {"Result": {"success": true, "iD": 1, "errorMessage": ""}}
```

### Update Comment

```
POST /taskmanager/TaskCommand.UpdateComment
Body: [{"taskID": 1, "commentIndex": 0, "content": "Updated text"}]
Response: {"Result": {"success": true, "iD": 1, "errorMessage": ""}}
```

Automatically sets `IsEdited := true` and updates `UpdatedAt`.

### Delete Comment

```
POST /taskmanager/TaskCommand.DeleteComment
Body: [1, 0]
Response: {"Result": {"success": true, "iD": 1, "errorMessage": ""}}
```

Parameters: `[aTaskID, aCommentIndex]` — removes comment at the specified zero-based index.

### View Comments (via GetTaskView)

```
POST /taskmanager/TaskQuery.GetTaskView
Body: [1]
Response: {
  "Result": {
    "iD": 1,
    "title": "My Task",
    "commentCount": 2,
    "comments": [
      {"content": "First comment", "author": "Alice", "createdAt": "...", "updatedAt": "...", "isEdited": false},
      {"content": "Second comment", "author": "Bob", "createdAt": "...", "updatedAt": "...", "isEdited": false}
    ]
  }
}
```

## Business Rules

1. **Task must exist**: Comments can only be added to existing tasks
2. **Content length**: 1 to 10,000 characters
3. **Author required**: Every comment must have an author name
4. **Timestamps**: CreatedAt and UpdatedAt are set automatically (UTC)
5. **Edit tracking**: UpdateComment sets `IsEdited := true`
6. **Index-based access**: Comments are identified by zero-based array index
7. **Cascade delete**: Deleting a task removes all its embedded comments automatically

## JavaScript Example

```javascript
// Add a comment
const result = await soaCall('TaskCommand', 'AddComment', [{
    TaskID: 1,
    Content: 'Great progress on this task!',
    Author: 'Developer'
}]);

if (result.Result.Success) {
    // Reload task to see updated comments
    const task = await soaCall('TaskQuery', 'GetTaskView', [1]);
    const comments = task.Result.Comments;
    comments.forEach((c, i) => {
        console.log(`[${i}] ${c.Author}: ${c.Content}`);
    });
}
```

## CLI Example

```bash
# Add a comment to task 1
./bin/cli_client comment 1 "This looks good!" "Developer"
```
