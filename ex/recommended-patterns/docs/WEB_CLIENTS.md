
# Web Clients — CQRS Integration

## Overview

The project ships **two** browser single-page applications under `static/`. Both
drive the CQRS SOA endpoints (no ORM/REST access) through the identical
`soaCall()` helper documented below:

| File | Title | Scope |
|------|-------|-------|
| `static/index.html` | *Task Manager* | Streamlined client — task create / list / complete / delete, status filtering, and a statistics dashboard. |
| `static/app.html`   | *Task Manager — Full Featured* | Superset of `index.html` that also exercises tags and comments: per-task detail view, tag creation and assignment, and comment add / delete. |

Both are served by the static file host at `http://localhost:8080/static/<file>`
(`/static` and `/static/` default to `index.html` — see `TStaticFileServer` in
`src/serv/app/ServAppTaskManager.pas`). The next two sections cover the shared SOA
call pattern and `index.html`; [`app.html`](#apphtml--full-featured-client) then
adds the tag/comment operations.

> This document covers the browser SPAs. The FPC command-line client
> (`src/cli_client.pas`) and its local/remote backends
> (`AppTaskManagerClient*.pas`) are described in `PROJECT_STRUCTURE.md`.

## `index.html` — features

- Task creation with title, description, priority (1-5), and due date
- Task list with filtering by status (All, Pending, In Progress, Completed)
- Task completion toggle and deletion
- Statistics dashboard (total, pending, completed)
- Tag count display per task
- Color-coded priority badges (Low through Critical)
- Responsive design with modern gradient UI

## API Integration

### SOA Call Pattern

All API calls use the `soaCall()` helper function:

```javascript
async function soaCall(service, method, params) {
    const url = `${API_BASE}/${service}.${method}`;
    const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(params)
    });
    return response.json();
}
```

**Endpoint format**: `POST /taskmanager/{ServiceName}.{MethodName}`

**Parameters**: JSON array (positional)

**Response**: `{"Result": <value>}` — unwrapped by `unwrapResult(data)` which reads `data.Result`

### Task Operations

```javascript
// Load tag cache for badge display
const tags = await soaCall('TagQuery', 'ListTags', []);

// List tasks (empty string = all, or "pending"/"in_progress"/"completed")
const tasks = await soaCall('TaskQuery', 'ListTasks', [status]);

// Create task via TTaskCreateDTO
// DTO keys are camelCase on the wire (scLowerCaseFirst, see shared_types.pas)
const result = await soaCall('TaskCommand', 'CreateTask', [{
    title: title,
    description: description,
    priority: parseInt(priority),
    dueDate: dueDate || ''
}]);

// Mark complete/incomplete
await soaCall('TaskCommand', 'MarkComplete', [taskId, true]);

// Delete task
await soaCall('TaskCommand', 'DeleteTask', [taskId]);
```

### Response Handling

Command operations return `TCommandResult`:
```javascript
const result = unwrapResult(data);
if (result.success) {
    showMessage('Task created!', 'success');
} else {
    showMessage(`Error: ${result.errorMessage}`, 'error');
}
```

Query operations return DTOs directly:
```javascript
const tasks = unwrapResult(data); // TTaskListItemDTO array
tasks.forEach(task => {
    // Fields are camelCase on the wire (scLowerCaseFirst):
    // task.iD, task.title, task.priority, task.priorityName,
    // task.status, task.isCompleted, task.dueDate,
    // task.commentCount, task.tagCount
});
```

## `app.html` — full-featured client

`app.html` reuses the same `soaCall()` / `unwrapResult()` helpers and wire contract
as `index.html`, and adds the tag- and comment-management surface on top of
everything `index.html` does. In total it drives all four services:

| Service | Methods used |
|---------|--------------|
| `TaskQuery`   | `ListTasks`, `GetTaskView` |
| `TaskCommand` | `CreateTask`, `MarkComplete`, `DeleteTask`, `AddTag`, `RemoveTag`, `AddComment`, `DeleteComment` |
| `TagQuery`    | `ListTags` |
| `TagCommand`  | `CreateTag` |

Operations beyond `index.html` (parameter shapes exactly as sent by `app.html`):

```javascript
// Open a task detail view — returns TTaskViewDTO (comments + assigned tag IDs)
const view = unwrapResult(await soaCall('TaskQuery', 'GetTaskView', [taskId]));

// Create a tag (TTagCreateDTO), then attach / detach it to a task (positional)
await soaCall('TagCommand',  'CreateTag', [{ name, color }]);
await soaCall('TaskCommand', 'AddTag',    [taskId, tagId]);
await soaCall('TaskCommand', 'RemoveTag', [taskId, tagId]);

// Comments live inside the task aggregate, addressed by index.
// AddComment takes a TTaskAddCommentDTO; DeleteComment is positional.
await soaCall('TaskCommand', 'AddComment',    [{ taskID, content, author }]);
await soaCall('TaskCommand', 'DeleteComment', [taskId, commentIndex]);
```

Comment fields on the wire are camelCase (`comment.author`, `comment.content`,
`comment.createdAt`, `comment.isEdited`). See `COMMENTS_FEATURE.md` for the DTO
shapes and business rules, and `API.md` for the full endpoint reference.

## Usage

1. Compile and run the server:
   ```bash
   ./compile.sh && ./run.sh
   ```

2. Open in browser:
   ```
   http://localhost:8080/static/index.html   # streamlined client
   http://localhost:8080/static/app.html     # full-featured client (tags + comments)
   ```

### Typical Workflow

1. **Create tasks** using the form (title, description, priority, due date)
2. **Filter tasks** by status using the filter buttons
3. **Complete tasks** using the Complete/Reopen button
4. **Delete tasks** with confirmation dialog
5. **View statistics** in the dashboard bar

## Technical Details

- Vanilla JavaScript (ES6+, no frameworks)
- CSS3 with gradients and hover animations
- Single HTML file, no external dependencies
- Fetch API for all CQRS calls
- Tag cache loaded at startup for badge display
