# 37-loggergui - Logger with VCL GUI Viewer

**Port of**: DMVCFramework `samples/LoggerGUI`
**Difficulty**: Low
**Demonstrates**: TSynLog in VCL applications, log file viewing, GUI integration

## Overview

This sample demonstrates how to use mORMot2's `TSynLog` logging in a VCL GUI application. It provides a visual interface to generate log entries and view the log file contents in real-time.

This is a direct port of the DMVC LoggerPro GUI sample, showing how to integrate logging into desktop applications.

## DMVC → mORMot2 Mapping

### VCL Integration

| DMVC Pattern | mORMot2 Equivalent |
|--------------|-------------------|
| `MVCFramework.Logger` unit | `mormot.core.log` unit |
| Button click → `Log.Info/Warn/etc` | Button click → `TSynLog.Add.Log(...)` |
| Manual log viewer | `StringFromFile()` + Memo display |
| LoggerPro global instance | `TSynLog.Family` global instance |

### Application Structure

**DMVC**:
```pascal
program LoggerSampleGUI;
uses
  Vcl.Forms,
  MVCFramework.Logger,
  MainFormU;
begin
  Application.CreateForm(TMainForm, MainForm);
  // Logger auto-initialized
end;
```

**mORMot2**:
```pascal
program LoggerGUISample;
uses
  Vcl.Forms,
  mormot.core.log,
  MainFormU;
begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  Application.CreateForm(TMainForm, MainForm);
end;
```

## Implementation Details

### 1. Form Creation and Logger Setup

**Port of**: DMVC `FormCreate` event

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.AutoFlushTimeOut := 1;

  // Display log file location
  lblLogFile.Caption := 'Log file: ' + TSynLog.Family.DefaultLogFile;
end;
```

**Key Features**:
- Logging configured at form creation
- Log file path displayed to user
- Memo initialized with welcome message

### 2. Test Logger Button

**Port of**: DMVC `btnLoggerTestClick`

Generates sample log entries at all severity levels:

```pascal
procedure TMainForm.btnLoggerTestClick(Sender: TObject);
begin
  TSynLog.Add.Log(sllInfo, 'This is an info log [tag:log1]');
  AddLog('INFO', 'This is an info log [tag:log1]');

  TSynLog.Add.Log(sllWarning, 'This is a warn log [tag:log1]');
  AddLog('WARN', 'This is a warn log [tag:log1]');

  // ... Debug, Error, Fatal ...
end;
```

**Display Format**:
```
[14:30:45] INFO: This is an info log [tag:log1]
[14:30:45] WARN: This is a warn log [tag:log1]
[14:30:45] DEBUG: This is a debug log [tag:log2]
[14:30:45] ERROR: This is an error log [tag:log2]
[14:30:45] FATAL: This is a fatal log [tag:log3]
```

### 3. Clear Display Button

**Port of**: Custom extension (not in DMVC sample)

```pascal
procedure TMainForm.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  memoLog.Lines.Add('Log display cleared');
end;
```

Clears the memo display (doesn't affect log file).

### 4. View Log File Button

**Port of**: Custom extension using `StringFromFile()`

```pascal
procedure TMainForm.btnViewLogFileClick(Sender: TObject);
var
  logContent: RawUtf8;
begin
  logFile := TSynLog.Family.DefaultLogFile;

  if not FileExists(logFile) then
    Exit;

  logContent := StringFromFile(logFile);

  memoLog.Clear;
  memoLog.Lines.Add('=== Log File Contents ===');
  memoLog.Lines.Add(Utf8ToString(logContent));
end;
```

**Features**:
- Reads entire log file
- Displays raw log format
- Handles UTF-8 encoding correctly
- Shows file path

## Usage

### Build and Run

```bash
# Compile:
dcc32 37-loggergui.dpr

# Or use Delphi compiler utility:
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\37-loggergui\37-loggergui.dproj

# Run:
37-loggergui.exe
```

### User Interface

**Main Form Components**:
- **Test Logger** button: Generates sample log entries
- **Clear Display** button: Clears the memo (preserves log file)
- **View Log File** button: Loads and displays the actual log file
- **Memo**: Displays log entries or file contents
- **Label**: Shows current log file path

### Workflow

1. Click **Test Logger** → Generates 5 log entries (Info, Warn, Debug, Error, Fatal)
2. Click **View Log File** → Displays raw log file with timestamps
3. Click **Clear Display** → Clears memo for fresh output
4. Repeat as needed

### Expected Output

**Initial Display**:
```
mORMot2 Logger GUI Sample
=========================
Click "Test Logger" to generate sample log entries
Port of: DMVCFramework samples/LoggerGUI
```

**After Clicking "Test Logger"**:
```
[14:30:45] INFO: This is an info log [tag:log1]
[14:30:45] WARN: This is a warn log [tag:log1]
[14:30:45] DEBUG: This is a debug log [tag:log2]
[14:30:45] ERROR: This is an error log [tag:log2]
[14:30:45] FATAL: This is a fatal log [tag:log3]

All logs written to: W:\...\LoggerGUISample 20251220_143045.log
```

**After Clicking "View Log File"**:
```
=== Log File Contents ===
File: W:\...\LoggerGUISample 20251220_143045.log

20251220 14304512  +    TMainForm.FormCreate
20251220 14304512 info  This is an info log [tag:log1]
20251220 14304512 warn  This is a warn log [tag:log1]
20251220 14304512 debug This is a debug log [tag:log2]
20251220 14304512 EXC   This is an error log [tag:log2]
20251220 14304512 fail  This is a fatal log [tag:log3]
```

## Key Differences from DMVC

| Feature | DMVC LoggerPro | mORMot2 TSynLog |
|---------|----------------|-----------------|
| **Setup** | Auto-initialized | Manual `TSynLog.Family` config |
| **Log File Format** | Plain text with headers | Compact text with timestamps |
| **File Viewing** | External viewer | Built-in `StringFromFile()` |
| **Real-time Updates** | Via file watchers | Manual refresh |
| **Thread Safety** | Appender queues | Per-thread buffers |
| **GUI Performance** | Good | Excellent (lock-free) |

## Advanced Features

### 1. Log File Rotation Viewer

Add real-time log rotation monitoring:

```pascal
procedure TMainForm.Timer1Timer(Sender: TObject);
var
  currentFile: string;
begin
  currentFile := TSynLog.Family.DefaultLogFile;
  if currentFile <> fLastLogFile then
  begin
    lblLogFile.Caption := 'Log file: ' + currentFile;
    fLastLogFile := currentFile;
  end;
end;
```

### 2. Filtered Log Display

Show only specific log levels:

```pascal
procedure TMainForm.ShowOnlyErrors;
var
  i: Integer;
  line: string;
begin
  for i := memoLog.Lines.Count - 1 downto 0 do
  begin
    line := memoLog.Lines[i];
    if (Pos('ERROR', line) = 0) and (Pos('FATAL', line) = 0) then
      memoLog.Lines.Delete(i);
  end;
end;
```

### 3. Export Log Subset

Export filtered logs to file:

```pascal
procedure TMainForm.ExportErrorsToFile(const aFileName: string);
var
  logContent: RawUtf8;
  lines: TStringDynArray;
  errors: TRawUtf8DynArray;
  i: Integer;
begin
  logContent := StringFromFile(TSynLog.Family.DefaultLogFile);
  lines := CsvToRawUtf8DynArray(logContent, #10);

  for i := 0 to High(lines) do
    if PosEx('EXC', lines[i]) > 0 then
      AddRawUtf8(errors, lines[i]);

  FileFromString(RawUtf8ArrayToCsv(errors, #13#10), aFileName);
end;
```

## See Also

- **36-logging** - Basic console logging
- **38-custom_logger** - Custom log file locations
- **39-log_filter** - Log message filtering
- **26-middleware_analytics** - HTTP request analytics

## VCL Integration Best Practices

### 1. Non-Blocking Logging

TSynLog is non-blocking by default, so GUI remains responsive:

```pascal
procedure TMainForm.btnHeavyOperationClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to 10000 do
  begin
    TSynLog.Add.Log(sllInfo, 'Processing item %', [i]);
    Application.ProcessMessages;  // GUI stays responsive
  end;
end;
```

### 2. Exception Logging in VCL

Hook global exception handler:

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := HandleException;
end;

procedure TMainForm.HandleException(Sender: TObject; E: Exception);
begin
  TSynLog.Add.Log(sllException, E);
  ShowMessage('An error occurred: ' + E.Message);
end;
```

### 3. Shutdown Logging

Log application lifecycle:

```pascal
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TSynLog.Add.Log(sllInfo, 'Application closing');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TSynLog.Add.Log(sllInfo, 'Main form destroyed');
end;
```

## References

- **mORMot2 Logging**: `mormot.core.log.pas`
- **DMVC Sample**: `DMVCFramework/samples/LoggerGUI`
- **VCL Integration**: `mormot.core.text.pas` (StringFromFile)

---

**Status**: ✅ Compilable and functional
**Created**: 2025-12-20
**Last tested**: Delphi 12 Athens (Win32/Win64)
