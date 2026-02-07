# UI Design Specification

## Form Properties

```
Width: 418
Height: 400
Position: poDesktopCenter
Font.Name: MS Sans Serif
Font.Height: -13
Color: clBtnFace
```

## Caption Format

```
Sample <NN> - <Description>
```

## Standard Layout (Phase 1)

```
┌─────────────────────────────────────┐
│  Your Name:                         │  Top: 16
│  ┌──────────┐  ┌─────────────────┐  │  Top: 36
│  │ EditName │  │ ButtonFind      │  │
│  └──────────┘  └─────────────────┘  │
│  ┌─────────────────────────────┐    │  Top: 72
│  │      MemoQuestion           │    │
│  └─────────────────────────────┘    │
│  Names:                             │  Top: 168
│  ┌─────────────────────────────┐    │  Top: 188
│  │                             │    │
│  │      ListNames              │    │
│  │                             │    │
│  └─────────────────────────────┘    │
│  ┌────┐ ┌────┐ ┌──────┐ ┌────┐     │  Top: 328
│  │New │ │Save│ │Delete│ │Quit│     │
│  └────┘ └────┘ └──────┘ └────┘     │
└─────────────────────────────────────┘
```

## Component Specifications

| Component | Left | Top | Width | Height | TabOrder |
|-----------|------|-----|-------|--------|----------|
| LabelName | 32 | 16 | 71 | 16 | - |
| EditName | 32 | 36 | 121 | 24 | 0 |
| ButtonFind | 184 | 36 | 161 | 25 | 1 |
| MemoQuestion | 32 | 72 | 313 | 81 | 2 |
| LabelNames | 32 | 168 | 50 | 16 | - |
| ListNames | 32 | 188 | 313 | 121 | 3 |
| ButtonNew | 32 | 328 | 70 | 25 | 4 |
| ButtonSave | 113 | 328 | 70 | 25 | 5 |
| ButtonDelete | 194 | 328 | 70 | 25 | 6 |
| ButtonQuit | 275 | 328 | 70 | 25 | 7 |

## Button Captions

```pascal
ButtonFind.Caption := 'Find';
ButtonNew.Caption := 'New';
ButtonSave.Caption := 'Save';
ButtonDelete.Caption := 'Delete';
ButtonQuit.Caption := 'Quit';
```

## Naming Convention

| Type | Prefix | Example |
|------|--------|---------|
| TLabel | Label | LabelName, LabelNames |
| TEdit | Edit | EditName |
| TMemo | Memo | MemoQuestion |
| TListBox | List | ListNames |
| TButton | Button | ButtonNew, ButtonSave, ButtonDelete, ButtonQuit |

## Event Handlers

```pascal
FormCreate          - Initialize Model, Client, load ListNames
FormDestroy         - Free Client, Model
ButtonFindClick     - Jump to Name in ListNames
ButtonNewClick      - Clear EditName and MemoQuestion
ButtonSaveClick     - Add (no selection) or Update (selection)
ButtonDeleteClick   - Confirm, then delete selected record
ButtonQuitClick     - Close form
ListNamesClick      - Show selected record in fields
```

## Private Methods

```pascal
function GetSelectedID: TID;   - Return ID of selected item (0 if none)
procedure RefreshNamesList;    - Reload ListNames from database
```

## Save Logic

```
if ListNames.ItemIndex >= 0 then
  Update existing record
else
  Add new record
```

## Delete Logic

```
if ListNames.ItemIndex < 0 then
  ShowMessage('No record selected')
else if MessageDlg confirms then
  Delete record and refresh list
```

## Margins

- Left margin: 32px
- Right edge alignment: 345px (32 + 313)
- Button spacing: 11px
