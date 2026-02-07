# mdLayout - Adaptive Layout Manager

## Overview

The **mdLayout** framework provides a generic, reusable layout system for Lazarus/FPC applications that works across all platforms without using anchors or RAD-style constraints.

## Key Features

- **Relative Positioning**: Place controls relative to each other
- **Proportional Spacing**: Spacing adapts to control sizes and screen resolutions
- **Platform Abstraction**: Automatic platform-specific fixes (macOS button heights, transparency, etc.)
- **Type-Safe**: Compile-time checking of layout operations
- **Fluent API**: Optional builder pattern for readable code
- **Auto-Sizing**: Forms automatically size to their content

## Quick Start

### Basic Example

```pascal
uses
  mdLayout;

procedure TMyForm.FormShow(Sender: TObject);
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
begin
  // Create margins based on a base height (usually a label or button height)
  Margins := LayoutMargins(Button1.Height);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    // Apply platform fixes
    Layout.AdjustForPlatform;

    // Place controls
    Layout.Place(Label1, Edit1, ldRight, 0.5);  // Edit1 to the right of Label1
    Layout.Place(Label1, Label2, ldBelow, 0.5); // Label2 below Label1

    // Auto-size the form
    Layout.AutoSizeForm;
  finally
    Layout.Free;
  end;
end;
```

## Core Types

### TLayoutDirection

Defines the direction for relative placement:

- `ldRight` - Place to the right
- `ldBelow` - Place below
- `ldAbove` - Place above
- `ldLeft` - Place to the left

### TLayoutAlign

Alignment options for `AlignTo`:

- `laLeft`, `laCenter`, `laRight` - Horizontal alignment
- `laTop`, `laCenter`, `laBottom` - Vertical alignment

### TLayoutSpacing

Defines spacing between controls:

```pascal
// Proportional spacing (multiplied by reference height)
LayoutSpacingProportional(0.5)

// Fixed pixel spacing
LayoutSpacingFixed(10)
```

### TLayoutMargins

Form margins:

```pascal
// Auto-calculated margins
Margins := LayoutMargins(BaseHeight);

// Custom margins
Margins := LayoutMarginsCustom(10, 10, 20, 20, 15);
// Left, Top, Right, Bottom, Middle
```

## Main Methods

### TLayoutHelper.Place

Place a control relative to another:

```pascal
// Simple placement with proportional spacing
Layout.Place(Reference, Target, ldBelow, 0.5);

// Placement with spacing object
Layout.Place(Reference, Target, ldRight, LayoutSpacingFixed(10));
```

### TLayoutHelper.PlaceColumn

Place multiple controls in a column:

```pascal
Layout.PlaceColumn(
  StartLabel,
  [Label2, Label3, Label4],
  LayoutSpacingProportional(0.5)
);
```

### TLayoutHelper.PlaceRow

Place multiple controls in a row:

```pascal
Layout.PlaceRow(
  StartButton,
  [OKButton, CancelButton, HelpButton],
  LayoutSpacingFixed(8)
);
```

### TLayoutHelper.PlaceSeparator

Place a separator (line/bevel) with asymmetric spacing - smaller space before, larger space after:

```pascal
// Place separator with default spacing (0.6 before, 1.5 after)
Layout.PlaceSeparator(LastLabel, SeparatorLine, FirstSystemLabel);

// Custom spacing
Layout.PlaceSeparator(LastLabel, SeparatorLine, FirstSystemLabel, 0.5, 2.0);
```

**Use Case**: Perfect for visual dividers in About dialogs, settings forms, or any UI where you want to separate sections with a line that has more breathing room after it than before it.

### TLayoutHelper.CalculateMaxWidth

Get maximum width from an array of controls:

```pascal
MaxWidth := Layout.CalculateMaxWidth([Label1, Label2, Label3]);
```

### TLayoutHelper.AutoSizeForm

Automatically resize form to fit content:

```pascal
// Auto-size with optional minimums
Layout.AutoSizeForm(400, 300);  // Min 400x300
Layout.AutoSizeForm;             // No minimums
```

### TLayoutHelper.AdjustForPlatform

Apply platform-specific fixes:

```pascal
Layout.AdjustForPlatform;  // Fixes macOS button heights, image transparency, etc.
```

## Advanced Examples

### Form with Label/Edit Pairs

```pascal
procedure TDataForm.FormShow(Sender: TObject);
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  RowTop: Integer;
begin
  Margins := LayoutMargins(EditName.Height);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    // First row
    LabelName.SetBounds(Margins.Left, Margins.Top, LabelName.Width, LabelName.Height);
    Layout.Place(LabelName, EditName, ldRight, LayoutSpacingFixed(10));

    // Second row
    Layout.Place(LabelName, LabelEmail, ldBelow, 1.0);
    Layout.Place(LabelEmail, EditEmail, ldRight, LayoutSpacingFixed(10));

    // Button row
    Layout.AutoSizeForm;
    OKButton.SetBounds(
      ClientWidth - Margins.Right - OKButton.Width,
      ClientHeight - Margins.Bottom - OKButton.Height,
      OKButton.Width, OKButton.Height
    );
    Layout.Place(OKButton, CancelButton, ldLeft, LayoutSpacingFixed(8));

  finally
    Layout.Free;
  end;
end;
```

### Using the Fluent API

```pascal
procedure TDialogForm.FormShow(Sender: TObject);
var
  Layout: TLayoutHelper;
  Builder: TLayoutBuilder;
  Margins: TLayoutMargins;
begin
  Margins := LayoutMargins(16);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    // Set initial position
    Label1.SetBounds(Margins.Left, Margins.Top, Label1.Width, Label1.Height);

    // Use builder for chain of placements
    Builder := TLayoutBuilder.Create(Layout, Label1);
    try
      Builder
        .Below(0.5).Add(Label2)
        .Below(0.5).Add(Label3)
        .Below(2.0).Add(ButtonOK)  // Larger gap before button
        .Done;
    finally
      Builder.Free;
    end;

    Layout.AutoSizeForm;
  finally
    Layout.Free;
  end;
end;
```

### Grid Layout

```pascal
procedure TGridForm.ArrangeInGrid;
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  Row, Col: Integer;
  CellWidth, CellHeight: Integer;
  CurrentControl: TControl;
begin
  Margins := LayoutMargins(20);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    Layout.AdjustForPlatform;

    CellWidth := 100;
    CellHeight := 30;

    for Row := 0 to 2 do
      for Col := 0 to 2 do
      begin
        CurrentControl := GridControls[Row * 3 + Col];
        CurrentControl.SetBounds(
          Margins.Left + Col * (CellWidth + 10),
          Margins.Top + Row * (CellHeight + 10),
          CellWidth,
          CellHeight
        );
      end;

    Layout.AutoSizeForm;
  finally
    Layout.Free;
  end;
end;
```

## Platform-Specific Behavior

The framework automatically handles platform differences:

### macOS (DARWIN)
- Button heights adjusted to 22 pixels
- Image transparency disabled (transparency issues on macOS)

### Windows/Linux
- Standard control heights preserved
- No special adjustments needed

## Best Practices

1. **Always call AdjustForPlatform early** in FormShow
2. **Use proportional spacing** for controls that scale with font size
3. **Use fixed spacing** for pixel-perfect layouts (buttons, toolbars)
4. **Calculate margins from control heights** for adaptive sizing
5. **Call AutoSizeForm** after placing all controls
6. **Free the Layout helper** in try/finally block

## Migration from Old Code

### Before (using custom Place function):

```pascal
procedure Place(const AReference, AComponent: TControl;
  ADirection: TDirection; ADistance: single);
var
  Left, Top: integer;
begin
  if ADirection = dirRight then
  begin
    Left := AReference.Left + AReference.Width + (round(ADistance * AReference.Height));
    Top := AReference.Top;
  end
  else if ADirection = dirBelow then
  begin
    Left := AReference.Left;
    Top := AReference.Top + AReference.Height + (round(ADistance * AReference.Height));
  end;
  AComponent.SetBounds(Left, Top, AComponent.Width, AComponent.Height);
end;
```

### After (using mdLayout):

```pascal
uses mdLayout;

var
  Layout: TLayoutHelper;
begin
  Layout := TLayoutHelper.Create(Self, LayoutMargins(16));
  try
    Layout.Place(Reference, Component, ldRight, 0.5);
    Layout.Place(Reference, Component, ldBelow, 0.5);
  finally
    Layout.Free;
  end;
end;
```

## See Also

- [rgAbout.pas](../rgabout.pas) - Real-world example of mdLayout usage
- [mdForms.pas](mdforms.pas) - Related form utilities
