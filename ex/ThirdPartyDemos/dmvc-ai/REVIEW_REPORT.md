# mORMot2 DMVC Adapted Examples Review

## Latest Update: 2024-12-23 - Refactoring Campaign Complete

All 7 pedagogical gaps identified in [CRYTICAL-REVIEW-REPORT.md](CRYTICAL-REVIEW-REPORT.md) have been successfully addressed:

- **03-routing**: Added RESTful routing with `TUriRouter` (path parameters)
- **04-renders**: Added raw content rendering examples (CSV, Binary without envelope)
- **05-datasets**: Added `TVirtualDataSet` bridge for VCL integration
- **15-middleware_staticfiles**: Migrated to native `THttpServer.Route.Static()`
- **18-file_upload**: Simplified with native `MultiPartFormDataDecode()`
- **25-action_filters**: Refactored to use `TServiceFactoryServer.OnMethodExecute`
- **31-simple_api_using_datasets**: Added `TDocVariant` dynamic manipulation

See individual example READMEs and refactoring documentation for implementation details.

---

## Example Status Summary

| Example | Working? | Equivalence | Quality | Changes Needed |
|---------|----------|-------------|---------|----------------|
| 01-basicdemo_server | Yes | Yes (SOA) | Good | None |
| 02-console_sample | Yes | Yes (SOA) | Good | None |
| 03-routing | Yes | Yes (RESTful + SOA) | Excellent | ✅ REFACTORED - Now includes RESTful path parameters |
| 04-renders | Yes | Yes (Raw + Envelope) | Excellent | ✅ REFACTORED - Added raw content rendering examples |
| 05-datasets | Yes | Yes (Arrays + VCL) | Excellent | ✅ REFACTORED - Added TVirtualDataSet bridge |
| 06-articles_crud_server | Yes | Yes (CRUD + ORM) | Good | None |
| 07-master_details | Yes | Yes (Manual Logic) | Excellent | None |
| 08-basicauth | Yes | Yes (Manual Middleware) | Good | None |
| 09-custom_auth | Yes | Yes (OnMethodExecute) | Good | None (Fixed: wired OnMethodAuth to TServiceFactoryServer.OnMethodExecute) |
| 10-jsonwebtoken | Yes | Yes (Thread-local claims) | Good | None (Fixed: services use actual JWT claims via CurrentJwtClaims()) |
| 11-ssl_server | Yes | Yes | Good | None |
| 12-middleware | Yes | Yes (Chain pattern) | Good | None (Fixed: each middleware saves/calls previous handler) |
| 13-middleware_cors | Yes | Yes | Good | None |
| 14-middleware_compression | Yes | Yes | Good | None |
| 15-middleware_staticfiles | Yes | Yes (Native Static) | Excellent | ✅ REFACTORED - Now uses THttpServer.Route.Static() |
| 16-serversentevents | Yes | Yes (True SSE) | Good | None (Fixed: true SSE streaming via socket, not polling) |
| 17-websocket_primer | Yes | Yes (Periodic heartbeats) | Good | None (Fixed: added SendPeriodicMessages with dynamic intervals) |
| 18-file_upload | Yes | Yes (Native Multipart) | Excellent | ✅ REFACTORED - Now uses MultiPartFormDataDecode() |
| 19-basicdemo_vclclient | Yes | Yes | Good | None |
| 20-controllers_register | Yes | Yes (Access Control) | Good | None (Fixed: uses OnMethodExecute to enable/disable services at runtime) |
| 21-restclient | Yes | Yes | Good | None |
| 22-custom_role_auth | Yes | Yes | Good | None |
| 23-ssl_client | Yes | Yes | Good | None |
| 24-hmac_auth | Yes | Yes (HMAC Server) | Good | None (Fixed: full HMAC auth server with signature verification, timestamp validation, replay attack prevention) |
| 25-action_filters | Yes | Yes (Method-Level) | Excellent | ✅ REFACTORED - Now uses OnMethodExecute for granular filtering |
| 26-middleware_analytics | Yes | Yes | Good | None |
| 27-middleware_trace | Yes | Yes | Good | None |
| 28-activerecord_restful_crud | Yes | Yes | Good | None |
| 29-activerecord_showcase | Yes | Yes | Good | None |
| 30-simple_api_using_mvcactiverecord | Yes | Yes | Good | None |
| 31-simple_api_using_datasets | Yes | Yes (TDocVariant) | Excellent | ✅ REFACTORED - Now demonstrates dynamic data manipulation |
| 32-jsonwebtoken_livevaliditywindow | Yes | Yes | Good | None |
| 33-jsonwebtoken_roleauth | Yes | Yes | Good | None |
| 34-session_file_based | Yes | Yes (User-keyed) | Good | None (Fixed: persist by user ID, restore on re-auth) |
| 35-sessions | Yes | Yes | Good | None |
| 36-logging | Yes | Yes (TSynLog) | Excellent | None |
| 37-loggergui | Yes | Yes (VCL) | Good | None |
| 38-custom_logger | Yes | Yes | Good | None |
| 39-log_filter | Yes | Yes (EchoCustom) | Good | None (Fixed: assigned OnLogFilter to TSynLogFamily.EchoCustom) |
| 40-custom_exception_handling | Yes | Yes | Good | None |
| 41-custom_exception_handling_using_controller | Yes | Yes | Good | None |
| 42-server_in_dll | Yes | Yes | Good | None |
| 43-windows_service | Yes | Yes | Good | None |
| 44-servercontainer | Yes | Yes | Good | None |
| 45-isapi | Yes | Yes (Full Bridge) | Good | None (Fixed: bridges requests to actual TRestServer via Uri() call) |
| 46-profiling | Yes | Yes | Good | None |
| 47-profiling_showcase | Yes | Yes | Good | None |
| 48-jsonwriterrenders | Yes | Yes | Good | Replaced by DTOs/automatic serialization |
| 49-render_binary_contents | Yes | Yes (Raw Binary) | Good | None (Fixed: /files/ and /download/ endpoints serve raw binary with proper Content-Type) |
| 50-angular | Yes | Yes | Good | None |
| 50-utilities_batch | Yes | N/A (New) | Excellent | None (Excellent mORMot showcase) |
| 51-complete_examples_final | Yes | Yes | Good | None |
| 51-react | Yes | Yes (Router) | Good | None |
| 52-concurrency_speed_test | Yes | Yes | Good | None |
| 53-datapump | Yes | Yes (Batch) | Good | None |
| 54-bloom_filter | Yes | Yes | Good | None |
| 55-objectpool | Yes | Yes | Good | None |
| 56-articles_crud_vcl_client | Yes | Yes | Good | None |
| 57-jsonrpc | Yes | Yes | Good | None |
