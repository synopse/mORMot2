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
│  │ NameEdit │  │ ButtonFind      │  │
│  └──────────┘  └─────────────────┘  │
│  ┌─────────────────────────────┐    │  Top: 72
│  │      QuestionMemo           │    │
│  └─────────────────────────────┘    │
│  Names:                             │  Top: 168
│  ┌─────────────────────────────┐    │  Top: 188
│  │                             │    │
│  │      NamesList              │    │
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
| LabelEdit | 32 | 16 | 71 | 16 | - |
| NameEdit | 32 | 36 | 121 | 24 | 0 |
| ButtonFind | 184 | 36 | 161 | 25 | 1 |
| QuestionMemo | 32 | 72 | 313 | 81 | 2 |
| LabelList | 32 | 168 | 50 | 16 | - |
| NamesList | 32 | 188 | 313 | 121 | 3 |
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
| TLabel | Label | LabelEdit, LabelList |
| TEdit | - | NameEdit |
| TMemo | - | QuestionMemo |
| TListBox | - | NamesList |
| TButton | Button | ButtonNew, ButtonSave, ButtonDelete, ButtonQuit |

## Event Handlers

```pascal
FormCreate          - Initialize Model, Client, load NamesList
FormDestroy         - Free Client, Model
ButtonFindClick     - Jump to Name in NamesList
ButtonNewClick      - Clear NameEdit and QuestionMemo
ButtonSaveClick     - Add (no selection) or Update (selection)
ButtonDeleteClick   - Delete selected record
ButtonQuitClick     - Close form
NamesListClick      - Show selected record in fields
```

## Save Logic

```
if NamesList.ItemIndex >= 0 then
  Update existing record
else
  Add new record
```

## Margins

- Left margin: 32px
- Right edge alignment: 345px (32 + 313)
- Button spacing: 11px
