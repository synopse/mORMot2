# mORMot2 Custom Exception Handling Sample

Port of DelphiMVCFramework `custom_exception_handling` sample to mORMot2.

## Overview

This sample demonstrates how to implement custom exception handling in mORMot2 REST services. It shows:

1. **Custom Exception Classes**: Creating rich exception types with additional properties (severity, error codes, diagnostic info)
2. **Global Exception Handler**: Setting up a server-level exception handler using `OnErrorFail`
3. **HTML Error Responses**: Generating formatted HTML error pages
4. **Exception Logging**: Automatic logging of exceptions with context

## Features

### Custom Exception Class

The `EMyException` class extends the standard `Exception` with:

- **Severity levels**: Fatal, Error, Warning, Information
- **Error codes**: Numeric error codes for programmatic handling
- **Details**: Additional error details
- **Diagnostics**: Diagnostic information for troubleshooting
- **Expression**: The expression/path that caused the error

### Exception Handler

The global exception handler:

- Catches all exceptions from service methods
- Generates HTML error pages with color-coded severity
- Logs exceptions to TSynLog
- Differentiates between custom and standard exceptions

## Project Structure

```
40-custom_exception_handling/
├── src/
│   ├── api.interfaces.pas    # EMyException class and IExceptionHandlingApi interface
│   ├── api.impl.pas          # Service implementation that raises exceptions
│   └── server.pas            # Server with custom exception handler
├── 40-custom_exception_handling.dpr    # Main program
├── 40-custom_exception_handling.dproj  # Delphi project
└── README.md                 # This file
```

## API Endpoints

### GET /root/ExceptionHandlingApi/Index
Returns a welcome message.

### GET /root/ExceptionHandlingApi/RaiseCustomError
Raises a custom `EMyException` with rich error information:
- Code: 25
- Severity: Fatal
- Details: "some real problem"
- Diagnostics: "Ensure Patient resource is valid"
- Expression: "Patient/Identifier/value"

### GET /root/ExceptionHandlingApi/RaiseStandardError
Raises a standard `Exception` to demonstrate basic error handling.

### GET /root/ExceptionHandlingApi/GetCustomer?aID=123
Returns an empty response (successful call without errors).

## Testing

### Start the Server
```bash
./40-custom_exception_handling
```

### Test Custom Exception
```bash
curl http://localhost:8080/root/ExceptionHandlingApi/RaiseCustomError
```

Expected HTML response with:
- Red error heading
- Error code: 25
- Severity: Fatal
- Details, diagnostics, and expression fields

### Test Standard Exception
```bash
curl http://localhost:8080/root/ExceptionHandlingApi/RaiseStandardError
```

Expected HTML response with simple error message.

### Test Normal Call
```bash
curl http://localhost:8080/root/ExceptionHandlingApi/Index
```

Returns welcome message without errors.

## Key Differences from DMVC

### Exception Handler Registration

**DMVC:**
```pascal
fMVC.SetExceptionHandler(lExceptionHandler);
```

**mORMot2:**
```pascal
fServer.OnErrorFail := HandleException;
```

### Exception Handler Signature

**DMVC:**
```pascal
procedure(E: Exception; SelectedController: TMVCController;
  WebContext: TWebContext; var ExceptionHandled: Boolean);
```

**mORMot2:**
```pascal
procedure HandleException(aContext: TRestServerUriContext;
  const aException: Exception; var aHandled: Boolean);
```

### HTML Response Generation

**DMVC:**
```pascal
WebContext.Response.Content := '<html>...</html>';
WebContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
WebContext.Response.StatusCode := HTTP_STATUS.InternalServerError;
```

**mORMot2:**
```pascal
aContext.Returns(lHtmlResponse, HTTP_SERVERERROR, False,
  HTML_CONTENT_TYPE_HEADER);
```

## Comparison with Sample 41

This sample (40) implements exception handling at the **server level** using `OnErrorFail`. All exceptions from all services are handled by a single global handler.

Sample 41 implements exception handling at the **service/controller level**, where each service can override `OnError` to provide custom handling.

Choose the approach that fits your architecture:
- **Sample 40 (this)**: Centralized error handling, consistent error responses
- **Sample 41**: Per-service error handling, different error formats per service

## Learn More

- [mORMot2 Error Handling](https://synopse.info/fossil/wiki?name=SQLite3+Framework)
- [Exception Classes in Delphi](https://docwiki.embarcadero.com/RADStudio/en/Exceptions)
- [DMVC Exception Handling](https://github.com/danieleteti/delphimvcframework)

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
