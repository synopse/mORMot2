# Implementation Plan

## Development Workflow

### 1. Plan

```
- Read CLAUDE.md
- Read CLAUDE.md of affected project
- Check docs/UI-DESIGN.md for UI changes
- Develop 2-3 alternative solutions
- Recommend best solution with reasoning
- List affected files
- WAIT for user decision
```

### 2. Implement

```
- Make code changes after user approval
- Follow existing code patterns
- Maintain Delphi 7 compatibility
```

### 3. Compile

```bash
${LAZBUILD_PATH} src/<Project>.lpi
```

Delphi tested separately by user.

### 4. Test

```
- Ask user to run application
- WAIT for user confirmation (OK)
- Fix issues if reported
```

### 5. Document

```
- Update CLAUDE.md if architecture changed
- Update IMPLEMENTATION-PLAN.md status
```

### 6. Commit

```
- Only after user confirms test OK
- Format: "<Type>: <Description>"
- Example: "Feature: Add delete button"
```

## Bug Fix Protocol

```
1. Research   - Check mORMot2/LCL documentation
2. Analyze    - Understand root cause, no assumptions
3. Propose    - 2-3 solution approaches
4. Wait       - User must confirm approach
5. Implement  - Only after approval
```

**Prohibited:**
- Code changes without user decision
- Quick fixes without analysis
- Assumptions without documentation check

## Rules

- One feature/fix per commit
- Always compile before commit
- Follow existing code patterns

## Phase 1: Question List

**Project:** 01-StandAloneORM

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 1.1 | Create UI design in UI-DESIGN.md | done |
| 1.2 | Implement TListBox for question list | done |
| 1.3 | Refresh logic after Add | done |
| 1.4 | Selection shows details in Memo | done |
| 1.5 | Add confirmation dialog before delete | done |

**Affected Files:**
- `docs/UI-DESIGN.md`
- `01-StandAloneORM/src/main.pas`
- `01-StandAloneORM/src/main.dfm`

---

**Project:** 02-HttpClientServerORM

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 2.1 | Rollout UI changes per UI-DESIGN.md | done |

**Affected Files:**
- `02-HttpClientServerORM/src/main.pas`
- `02-HttpClientServerORM/src/main.dfm`

---

**Project:** 03-MethodBasedServices

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 3.1 | Rollout UI changes per UI-DESIGN.md | planned |

**Affected Files:**
- `03-MethodBasedServices/src/main.pas`
- `03-MethodBasedServices/src/main.dfm`

---

**Project:** 04-InterfacedBasedServices

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 4.1 | Rollout UI changes per UI-DESIGN.md | planned |

**Affected Files:**
- `04-InterfacedBasedServices/src/main.pas`
- `04-InterfacedBasedServices/src/main.dfm`

---

**Project:** 05-HttpDaemonORM

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 5.1 | Rollout UI changes per UI-DESIGN.md | planned |

**Affected Files:**
- `05-HttpDaemonORM/src/main.pas`
- `05-HttpDaemonORM/src/main.dfm`

---

**Project:** 06-DomainDrivenDesign

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 6.1 | Rollout UI changes per UI-DESIGN.md | planned |

**Affected Files:**
- `06-DomainDrivenDesign/src/main.pas`
- `06-DomainDrivenDesign/src/main.dfm`

---

**Project:** 07-HttpDockerORM

**Tasks:**

| # | Task | Status |
|---|------|--------|
| 7.1 | Rollout UI changes per UI-DESIGN.md | planned |

**Affected Files:**
- `07-HttpDockerORM/src/main.pas`
- `07-HttpDockerORM/src/main.dfm`

## Planned Features

| Priority | Project | Feature | Status |
|----------|---------|---------|--------|
| 1 | 01-StandAloneORM | Question List (TListBox) | active |

## Planned Changes

| Priority | Project | Change | Status |
|----------|---------|--------|--------|
| - | - | - | - |

## Known Bugs

| Priority | Project | Bug | Status |
|----------|---------|-----|--------|
| - | - | - | - |

## Status Values

```
planned  - Not started
active   - In progress
testing  - Awaiting user test
done     - Completed and verified
blocked  - Waiting for dependency
```

## File Change Protocol

```
New file:      Add to CLAUDE.md file list
Renamed file:  Update all references
Deleted file:  Remove from CLAUDE.md
New unit:      Add to uses clause, update CLAUDE.md
```
