# mORMot User Interface Units

## Folder Content

This folder holds the User Interface related features of the *mORMot* Open Source framework, version 2.


## Units Presentation

### mormot.ui.core

Basic types and reusable functions for VCL/LCL User Interface support
- Some LCL/VCL cross-compatibility definitions
- High-Level UI Wrapper Functions

### mormot.ui.controls

Some Custom Visual Components
- `THintWindowDelayed` as auto-hiding `THintWindow` descendant
- `TSynLabeledEdit` as extended `TLabeledEdit`
- `TUIComponentsPersist` to persist UI components as JSON

### mormot.ui.grid.orm

Fill a Read/Only `TDrawGrid`
- `TOrmTableToGrid` wrapper to manage a `TDrawGrid` from a `TOrmTable`
- Fill a `TStringGrid` from ORM results

### mormot.ui.gdiplus

VCL/LCL Windows GDI+ Graphics Device Interface Support
- `TSynPicture` and associated GIF/PNG/TIFF/JPG classes
- High-Level Function Wrappers to Manage Pictures
