# mORMot2 Custom Exception Handling Using Controller Sample

Port of DelphiMVCFramework `exception_handling_with_controller` sample to mORMot2.

## Overview

This sample demonstrates how to implement **controller/service-level** exception handling in mORMot2 REST services. Unlike sample 40 which uses a global exception handler, this approach gives each service control over its own error handling.

## Features

### Service-Level Exception Handling

The key feature is overriding the `OnError` method in the service implementation:

```pascal
function TExceptionHandlingApiService.OnError(E: Exception;
  var Call: TRestUriParams): Boolean;
```

This allows:
- **Per-service error handling**: Different services can handle exceptions differently
- **Content negotiation**: Return HTML or JSON based on client preferences
- **Custom error formatting**: Each service controls its error response format
- **Selective handling**: Services can choose which exceptions to handle and which to pass to default handler

### Content Negotiation

The service checks the client's preferred content type:
- If client accepts JSON → Use default JSON error handler
- Otherwise → Return custom HTML error page

## Project Structure

```
41-custom_exception_handling_using_controller/
├── src/
│   ├── api.interfaces.pas    # IExceptionHandlingApi interface
│   ├── api.impl.pas          # Service with OnError override
│   └── server.pas            # Server without global exception handler
├── 41-custom_exception_handling_using_controller.dpr    # Main program
├── 41-custom_exception_handling_using_controller.dproj  # Delphi project
└── README.md                 # This file
```

## API Endpoints

### GET /root/ExceptionHandlingApi/Index
Returns a welcome message.

### GET /root/ExceptionHandlingApi/RaiseServiceError
Raises a `EServiceException` (mORMot2's equivalent to DMVC's `EMVCException`):
- HTTP Status: 500
- Color: Red (status >= 500)
- Handled by service's `OnError` method

### GET /root/ExceptionHandlingApi/RaiseStandardError
Raises a standard `Exception`:
- HTTP Status: 500
- Color: Red
- Generic error message

### GET /root/ExceptionHandlingApi/GetCustomer?aID=123
Returns an empty response (successful call without errors).

## Testing

### Start the Server
```bash
./41-custom_exception_handling_using_controller
```

### Test with HTML Response (Default)
```bash
curl http://localhost:8080/root/ExceptionHandlingApi/RaiseServiceError
```

Expected: HTML error page with colored heading based on HTTP status code.

### Test with JSON Response
```bash
curl -H "Accept: application/json" \
  http://localhost:8080/root/ExceptionHandlingApi/RaiseServiceError
```

Expected: JSON error response from default mORMot2 error handler.

### Test Standard Exception
```bash
curl http://localhost:8080/root/ExceptionHandlingApi/RaiseStandardError
```

Expected: HTML error page with generic error message.

## Key Differences from DMVC

### Service-Level Exception Handler

**DMVC:**
```pascal
procedure TMyController.OnException(const aContext: TWebContext;
  const aException: Exception; var aHandled: Boolean);
```

**mORMot2:**
```pascal
function TExceptionHandlingApiService.OnError(E: Exception;
  var Call: TRestUriParams): Boolean;
```

### Content Type Check

**DMVC:**
```pascal
if Context.Request.ClientPrefer(TMVCMediaType.APPLICATION_JSON) then
begin
  aHandled := False;
  Exit;
end;
```

**mORMot2:**
```pascal
if IdemPropNameU(Call.OutHead, JSON_CONTENT_TYPE_VAR) then
begin
  Result := False; // Let default handler process
  Exit;
end;
```

### Response Generation

**DMVC:**
```pascal
aContext.Response.ContentType := TMVCMediaType.TEXT_HTML;
aContext.Response.Content := lHtmlResponse;
aContext.Response.StatusCode := lStatusCode;
```

**mORMot2:**
```pascal
Call.OutBody := lHtmlResponse;
Call.OutHead := HTML_CONTENT_TYPE_HEADER;
Call.OutStatus := lStatusCode;
```

## Architecture Comparison

### Sample 40 (Global Handler)
- **Approach**: Single exception handler at server level (`OnErrorFail`)
- **Scope**: Handles exceptions from ALL services
- **Use case**: Consistent error handling across entire API
- **Pros**: Centralized, consistent, easy to maintain
- **Cons**: Less flexible per-service customization

### Sample 41 (Controller/Service Handler)
- **Approach**: Each service overrides `OnError` method
- **Scope**: Handles exceptions only for that service
- **Use case**: Different error formats for different services
- **Pros**: Flexible, service-specific error handling
- **Cons**: More code, potential inconsistency

## When to Use Each Approach

### Use Sample 40 (Global Handler) when:
- You want consistent error responses across all endpoints
- You have a simple, uniform API structure
- You want centralized error logging/monitoring
- You're building a microservice with a single responsibility

### Use Sample 41 (Service Handler) when:
- Different services need different error formats
- You need content negotiation (HTML vs JSON vs XML)
- Services have different error severity classifications
- You're migrating from DMVC controller-based error handling
- You want fine-grained control per service

## Learn More

- [mORMot2 Service Error Handling](https://synopse.info/fossil/wiki?name=SQLite3+Framework)
- [TInjectableObjectRest.OnError](https://synopse.info/files/html/api-1.12/mormot.rest.core.html)
- [DMVC Controller Exception Handling](https://github.com/danieleteti/delphimvcframework)

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
