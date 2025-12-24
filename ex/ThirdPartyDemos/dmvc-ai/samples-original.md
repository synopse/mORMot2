# DMVC Framework - Complete Samples Inventory

**Total Samples**: 110 working samples (+ 3 utility folders)

**Last Updated**: 2025-12-19

**Source**: `/mnt/w/DMVCframework/samples/`

---

## Table of Contents

1. [Core Concepts](#core-concepts) (6 samples)
2. [Routing](#routing) (1 sample)
3. [Authentication & Authorization](#authentication--authorization) (3 samples)
4. [Middleware](#middleware) (10 samples)
5. [Database - ActiveRecord & Repository](#database---activerecord--repository) (7 samples)
6. [Database - General](#database---general) (4 samples)
7. [Server-Side Views & HTMX](#server-side-views--htmx) (9 samples)
8. [WebSockets](#websockets) (5 samples)
9. [Server-Sent Events (SSE)](#server-sent-events-sse) (3 samples)
10. [JSON-RPC](#json-rpc) (1 sample)
11. [JWT Tokens](#jwt-tokens) (4 samples)
12. [API Documentation (Swagger)](#api-documentation-swagger) (5 samples)
13. [Session Management](#session-management) (2 samples)
14. [Logging](#logging) (5 samples)
15. [Error Handling](#error-handling) (2 samples)
16. [Security](#security) (5 samples)
17. [Frontend Integration](#frontend-integration) (2 samples)
18. [Deployment & Hosting](#deployment--hosting) (5 samples)
19. [Dependency Injection](#dependency-injection) (2 samples)
20. [Caching & Redis](#caching--redis) (1 sample)
21. [Performance & Profiling](#performance--profiling) (2 samples)
22. [Monitoring & Metrics](#monitoring--metrics) (1 sample)
23. [File Operations](#file-operations) (1 sample)
24. [Rendering & Content](#rendering--content) (3 samples)
25. [Async Operations](#async-operations) (1 sample)
26. [Advanced Patterns](#advanced-patterns) (4 samples)
27. [Utilities & Helpers](#utilities--helpers) (7 samples)
28. [Complete Examples](#complete-examples) (5 samples)
29. [Miscellaneous](#miscellaneous) (11 samples)

---

## Core Concepts

### 1. basicdemo_server
**Path**: `samples/basicdemo_server/`
**Projects**: `BasicDemo.dpr`, `BasicDemo.dproj`
**Key Files**:
- `App1MainControllerU.pas` - Main controller implementation
- `WebModuleUnit1.pas` - Web module configuration
**Description**: Minimal REST server setup demonstrating basic DMVC architecture
**Category**: Getting Started

### 2. basicdemo_vclclient
**Path**: `samples/basicdemo_vclclient/`
**Projects**: `BasicDemoVCLClient.dpr`, `BasicDemoVCLClient.dproj`
**Description**: VCL client application demonstrating REST API consumption
**Category**: Getting Started

### 3. console_sample
**Path**: `samples/console_sample/`
**Projects**: `ConsoleDemo.dpr`, `ConsoleSample.dpr`
**Description**: Console-based DMVC server examples
**Category**: Getting Started

### 4. controllers_register
**Path**: `samples/controllers_register/`
**Projects**: `ServerWithControllersRegister.dpr`, `ServerWithControllersRegister.dproj`
**Key Files**:
- `MyController1U.pas` - First controller example
- `MyController2U.pas` - Second controller example
- `WebModuleMainU.pas` - Main web module
**Description**: Demonstrates multiple controller registration patterns
**Category**: Architecture

### 5. functional_actions_showcase
**Path**: `samples/functional_actions_showcase/`
**Projects**: `function_actions_showcase.dpr`, `function_actions_showcase.dproj`
**Key Files**:
- `ControllerU.pas` - Functional actions implementation
- `WebModuleU.pas` - Web module setup
**Description**: Functional programming patterns in DMVC actions
**Category**: Advanced Concepts

### 6. strongly_typed_actions
**Path**: `samples/strongly_typed_actions/`
**Projects**: `strongly_typed_actions.dpr`, `strongly_typed_actions.dproj`
**Key Files**:
- `MyControllerU.pas` - Strongly-typed action methods
- `WebModuleU.pas` - Web module configuration
**Description**: Type-safe action parameter binding
**Category**: Type Safety

---

## Routing

### 7. routing
**Path**: `samples/routing/`
**Projects**: `routingsample.dpr`, `routingsample.dproj`
**Key Files**:
- `RoutingSampleControllerU.pas` - Advanced routing patterns
- `WebModuleU.pas` - Route configuration
**Description**: Route parameters, patterns, and advanced routing techniques
**Category**: Core Feature

---

## Authentication & Authorization

### 8. custom_auth
**Path**: `samples/custom_auth/`
**Projects**: `CustomAuthServer.dpr`, `CustomAuthClient.dpr`
**Key Files**:
- `MyWebModuleU.pas` - Custom auth implementation
- `PrivateControllerU.pas` - Protected endpoints
- `PublicControllerU.pas` - Public endpoints
**Description**: Custom authentication mechanism implementation
**Category**: Security

### 9. custom_role_auth
**Path**: `samples/custom_role_auth/`
**Projects**: `CustomRoleAuthServer.dpr`, `CustomRoleAuthClient.dpr`
**Key Files**:
- `MyWebModuleU.pas` - Role-based auth implementation
- `PrivateControllerU.pas` - Role-protected endpoints
**Description**: Role-based authorization with custom logic
**Category**: Security

### 10. jsonwebtoken_roleauth
**Path**: `samples/jsonwebtoken_roleauth/`
**Projects**: `JWTRoleAuthServer.dpr`, `JWTClient.dpr`
**Key Files**:
- `AppControllerU.pas` - JWT role auth controller
- `WebModuleUnit1.pas` - JWT configuration
**Description**: JWT-based role authorization
**Category**: Security

---

## Middleware

### 11. middleware
**Path**: `samples/middleware/`
**Projects**: `MiddlewareSamples.dpr`, `MiddlewareSamples.dproj`
**Key Files**:
- `AppControllerU.pas` - Controller with middleware
- `WebModuleUnit1.pas` - Middleware registration
**Description**: General middleware implementation patterns
**Category**: Core Feature

### 12. middleware_activerecord
**Path**: `samples/middleware_activerecord/`
**Projects**: `middleware_activerecord.dpr`, `middleware_activerecord.dproj`
**Key Files**:
- `MainControllerU.pas` - ActiveRecord middleware integration
- `WebModuleU.pas` - Middleware setup
**Description**: ActiveRecord connection management via middleware
**Category**: Database

### 13. middleware_analytics
**Path**: `samples/middleware_analytics/`
**Projects**: `middleware_analytics.dpr`, `middleware_analytics.dproj`
**Key Files**:
- `MainControllerU.pas` - Analytics tracking
- `WebModuleU.pas` - Analytics middleware
**Description**: Request analytics and tracking middleware
**Category**: Monitoring

### 14. middleware_basicauthentication
**Path**: `samples/middleware_basicauthentication/`
**Projects**: `AuthenticateAuthorize.dpr`, `AuthenticationAuthorizationClient.dpr`
**Key Files**:
- `AppControllerU.pas` - Protected endpoints
- `WebModuleUnit1.pas` - Basic auth middleware
**Description**: HTTP Basic Authentication middleware
**Category**: Security

### 15. middleware_compression
**Path**: `samples/middleware_compression/`
**Projects**: `OutputCompressionSample.dpr`, `OutputCompressionSample.dproj`
**Key Files**:
- `MainControllerU.pas` - Response compression
- `WebModuleU.pas` - Compression middleware
**Description**: Automatic response compression (gzip, deflate)
**Category**: Performance

### 16. middleware_cors
**Path**: `samples/middleware_cors/`
**Projects**: `middleware_cors.dpr`, `SimpleWebServer.dpr`
**Key Files**:
- `MainControllerU.pas` - CORS-enabled endpoints
- `WebModuleU.pas` - CORS configuration
**Description**: Cross-Origin Resource Sharing (CORS) middleware
**Category**: Security

### 17. middleware_etag
**Path**: `samples/middleware_etag/`
**Projects**: `middleware_etag.dpr`, `middleware_etag.dproj`
**Key Files**:
- `App1MainControllerU.pas` - ETag-enabled endpoints
- `WebModuleUnit1.pas` - ETag middleware
**Description**: HTTP ETag caching middleware
**Category**: Performance

### 18. middleware_jwtblacklist
**Path**: `samples/middleware_jwtblacklist/`
**Projects**: `JWTBlacklistServerSample.dpr`, `JWTBlacklistClientSample.dpr`
**Key Files**:
- `AppControllerU.pas` - JWT blacklist implementation
- `WebModuleUnit1.pas` - JWT blacklist middleware
**Description**: JWT token blacklist/revocation
**Category**: Security

### 19. middleware_ratelimit_memory
**Path**: `samples/middleware_ratelimit_memory/`
**Projects**: `RateLimitSample.dpr`, `RateLimitSample.dproj`
**Key Files**:
- `MainControllerU.pas` - Rate-limited endpoints
- `WebModuleU.pas` - In-memory rate limiting
**Description**: Memory-based API rate limiting
**Category**: Performance
**Has README**: Yes

### 20. middleware_ratelimit_redis
**Path**: `samples/middleware_ratelimit_redis/`
**Projects**: `RateLimitRedisSample.dpr`, `RateLimitRedisSample.dproj`
**Key Files**:
- `MainControllerU.pas` - Rate-limited endpoints
- `WebModuleU.pas` - Redis-backed rate limiting
**Description**: Redis-based distributed rate limiting
**Category**: Performance
**Has README**: Yes

### 21. middleware_staticfiles
**Path**: `samples/middleware_staticfiles/`
**Projects**: `middleware_staticfiles.dpr`, `middleware_staticfiles.dproj`
**Key Files**:
- `MainControllerU.pas` - API endpoints
- `SPARedirectController.pas` - SPA fallback
- `WebModuleU.pas` - Static file serving
**Description**: Static file serving middleware with SPA support
**Category**: Deployment

### 22. middleware_trace
**Path**: `samples/middleware_trace/`
**Projects**: `middleware_trace.dpr`, `middleware_trace.dproj`
**Key Files**:
- `MainControllerU.pas` - Traced endpoints
- `WebModuleU.pas` - Request tracing
**Description**: Request/response tracing middleware
**Category**: Debugging

---

## Database - ActiveRecord & Repository

### 23. activerecord_restful_crud
**Path**: `samples/activerecord_restful_crud/`
**Projects**: `activerecord_restful_crud.dpr`, `activerecord_restful_crud.dproj`
**Key Files**:
- `OtherControllerU.pas` - RESTful CRUD controller
- `WebModuleU.pas` - ActiveRecord setup
**Description**: RESTful CRUD operations using ActiveRecord
**Category**: Database Patterns

### 24. activerecord_showcase
**Path**: `samples/activerecord_showcase/`
**Projects**: `activerecord_showcase.dpr`, `activerecord_showcase.dproj`
**Description**: Comprehensive ActiveRecord features showcase
**Category**: Database Patterns

### 25. repository_showcase
**Path**: `samples/repository_showcase/`
**Projects**: `repository_showcase.dpr`, `repository_showcase.dproj`
**Description**: Repository pattern implementation
**Category**: Database Patterns

### 26. simple_api_using_mvcactiverecord
**Path**: `samples/simple_api_using_mvcactiverecord/`
**Projects**: `SimpleRESTAPIUsingActiveRecord.dpr`
**Key Files**:
- `CustomersControllerU.pas` - Customer CRUD using ActiveRecord
**Description**: Simple REST API with ActiveRecord
**Category**: Getting Started

### 27. simple_api_using_mvcactiverecord_with_injection
**Path**: `samples/simple_api_using_mvcactiverecord_with_injection/`
**Projects**: `SimpleRESTAPIUsingInjection.dpr`
**Key Files**:
- `CustomersControllerU.pas` - DI-based ActiveRecord controller
**Description**: ActiveRecord with dependency injection
**Category**: Advanced Patterns

### 28. simple_api_using_mvcactiverecord_with_version
**Path**: `samples/simple_api_using_mvcactiverecord_with_version/`
**Projects**: `SimpleVersionedRESTAPIUsingActiveRecord.dpr`
**Key Files**:
- `CustomersControllerU.pas` - Versioned API with ActiveRecord
**Description**: API versioning with ActiveRecord
**Category**: API Design

### 29. simple_api_using_repository_with_injection
**Path**: `samples/simple_api_using_repository_with_injection/`
**Projects**: `RESTAPIUsingRepository.dpr`
**Key Files**:
- `Controller.CustomersU.pas` - Repository-based controller
**Description**: Repository pattern with dependency injection
**Category**: Database Patterns

---

## Database - General

### 30. ado
**Path**: `samples/ado/`
**Projects**: `ado_sample.dpr`, `ado_sample.dproj`
**Key Files**:
- `ControllerU.pas` - ADO integration
- `WebModuleU.pas` - ADO setup
**Description**: ADO database connectivity example
**Category**: Database Connectivity

### 31. datasets
**Path**: `samples/datasets/`
**Projects**: `DataSetUtilsSample.dpr`, `DataSetUtilsSample.dproj`
**Description**: TDataSet utilities and helpers
**Category**: Database Utilities

### 32. master_details
**Path**: `samples/master_details/`
**Projects**: `masterdetailssample.dpr`, `masterdetailssample.dproj`
**Key Files**:
- `Controllers.Base.pas` - Base controller
- `Controllers.Orders.pas` - Master-detail controller
- `WebModuleUnit1.pas` - Web module
**Description**: Master-detail relationship handling
**Category**: Database Patterns

### 33. simple_api_using_datasets
**Path**: `samples/simple_api_using_datasets/`
**Projects**: `SimpleRESTAPIUsingDatasets.dpr`
**Key Files**:
- `CustomersControllerU.pas` - TDataSet-based CRUD
**Description**: Simple REST API using TDataSet
**Category**: Getting Started

---

## Server-Side Views & HTMX

### 34. htmx
**Path**: `samples/htmx/`
**Projects**: `HTMX_Sample.dpr`, `HTMX_Sample.dproj`
**Key Files**:
- `uBase.Controller.pas` - Base HTMX controller
- `uMovie.Controller.pas` - Movie management with HTMX
**Description**: HTMX integration basics
**Category**: Interactive UI

### 35. htmx_mustache
**Path**: `samples/htmx_mustache/`
**Projects**: `htmx_mustache.dpr`, `htmx_mustache.dproj`
**Key Files**:
- `WebModuleU.pas` - Web module setup
- `WebSiteControllerU.pas` - Mustache + HTMX
**Description**: HTMX with Mustache template engine
**Category**: Template Engines

### 36. htmx_templatepro
**Path**: `samples/htmx_templatepro/`
**Projects**: `htmx_templatepro.dpr`, `htmx_templatepro.dproj`
**Key Files**:
- `WebModuleU.pas` - Web module setup
- `WebSiteControllerU.pas` - TemplatePro + HTMX
**Description**: HTMX with TemplatePro engine
**Category**: Template Engines

### 37. htmx_website_with_templatepro
**Path**: `samples/htmx_website_with_templatepro/`
**Projects**: `htmx_website_with_templatepro.dpr`
**Key Files**:
- `ControllerU.pas` - Website controller
- `WebModuleU.pas` - TemplatePro configuration
**Description**: Complete HTMX website with TemplatePro
**Category**: Complete Examples

### 38. htmx_website_with_webstencils
**Path**: `samples/htmx_website_with_webstencils/`
**Projects**: `htmx_website_with_webstencils.dpr`
**Key Files**:
- `ControllerU.pas` - Website controller
- `WebModuleU.pas` - WebStencils configuration
**Description**: Complete HTMX website with WebStencils
**Category**: Complete Examples

### 39. instant_search_with_htmx_and_templatepro
**Path**: `samples/instant_search_with_htmx_and_templatepro/`
**Projects**: `BooksSearch.dpr`, `BooksSearch.dproj`
**Key Files**:
- `Controllers.BooksU.pas` - Search controller
- `WebModuleU.pas` - Web module setup
**Description**: Real-time search with HTMX and TemplatePro
**Category**: Interactive UI

### 40. serversideviews_mustache
**Path**: `samples/serversideviews_mustache/`
**Projects**: `ServerSideViewsMustache.dpr`
**Key Files**:
- `WebModuleU.pas` - Web module
- `WebSiteControllerU.pas` - Mustache views
**Description**: Server-side rendering with Mustache
**Category**: Template Engines

### 41. serversideviews_templatepro
**Path**: `samples/serversideviews_templatepro/`
**Projects**: `ServerSideViewsTemplatePro.dpr`
**Key Files**:
- `WebModuleU.pas` - Web module
- `WebSiteControllerU.pas` - TemplatePro views
**Description**: Server-side rendering with TemplatePro
**Category**: Template Engines

### 42. templatepro_json
**Path**: `samples/templatepro_json/`
**Projects**: `templatepro_json_sample.dpr`
**Key Files**:
- `ControllerU.pas` - JSON template controller
- `WebModuleU.pas` - Web module
**Description**: JSON rendering with TemplatePro
**Category**: Template Engines
**Has README**: Yes

---

## WebSockets

### 43. websocket_chat
**Path**: `samples/websocket_chat/`
**Projects**: `WebSocketChatServer.dpr`, `WebSocketChatServer.dproj`
**Key Files**:
- `WebModuleU.pas` - WebSocket chat implementation
**Description**: Real-time chat server using WebSockets
**Category**: Real-time Communication

### 44. websocket_client_sample
**Path**: `samples/websocket_client_sample/`
**Projects**: `WebSocketClientVCL.dpr`, `WebSocketClientVCL.dproj`
**Description**: VCL WebSocket client example
**Category**: Real-time Communication
**Has README**: Yes

### 45. websocket_groups
**Path**: `samples/websocket_groups/`
**Projects**: `WebSocketGroupServer.dpr`, `WebSocketGroupServer.dproj`
**Description**: WebSocket group/room management
**Category**: Real-time Communication

### 46. websocket_javascript_client_sample
**Path**: `samples/websocket_javascript_client_sample/`
**Projects**: `WebSocketStockSample.dpr`
**Key Files**:
- `ControllerU.pas` - WebSocket controller
- `WebModuleU.pas` - Web module
**Description**: WebSocket server with JavaScript client
**Category**: Real-time Communication

### 47. websocket_primer
**Path**: `samples/websocket_primer/`
**Projects**: `WebSocketClientTest.dpr`, `WebSocketServerEcho.dpr`
**Description**: WebSocket basics - echo server and client
**Category**: Getting Started

---

## Server-Sent Events (SSE)

### 48. serversentevent_for_indy_based_servers
**Path**: `samples/serversentevent_for_indy_based_servers/`
**Projects**: `SSEIndyBasedSample.dpr`, `SSEClientViewer.dpr`
**Key Files**:
- `SSEControllerU.pas` - SSE controller for Indy
- `WebModuleU.pas` - Web module
**Description**: Server-Sent Events for Indy-based servers
**Category**: Real-time Communication

### 49. serversentevents
**Path**: `samples/serversentevents/`
**Projects**: `SSESample.dpr`, `SSESample.dproj`
**Key Files**:
- `SSEControllerU.pas` - SSE controller
- `WebModuleU.pas` - Web module
**Description**: Server-Sent Events basic implementation
**Category**: Real-time Communication

### 50. serversentevents2
**Path**: `samples/serversentevents2/`
**Projects**: `serversentevents2.dpr`, `serversentevents2sender.dpr`, `serversentevents2viewer.dpr`
**Key Files**:
- `StatusControllerU.pas` - SSE status controller
- `WebModuleU.pas` - Web module
**Description**: Advanced SSE with sender/viewer pattern
**Category**: Real-time Communication

---

## JSON-RPC

### 51. jsonrpc
**Path**: `samples/jsonrpc/`
**Projects**: `jsonrpcserver.dpr`, `jsonrpcclient_async.dpr`, `jsonrpcclient_sync.dpr`
**Key Files**:
- `MainWebModuleU.pas` - JSON-RPC web module
**Description**: JSON-RPC 2.0 server and clients (sync/async)
**Category**: RPC

---

## JWT Tokens

### 52. jsonwebtoken
**Path**: `samples/jsonwebtoken/`
**Projects**: `JWTServer.dpr`, `JWTClient.dpr`
**Key Files**:
- `AppControllerU.pas` - JWT auth controller
- `WebModuleUnit1.pas` - JWT configuration
**Description**: JWT authentication basics
**Category**: Security

### 53. jsonwebtoken_livevaliditywindow
**Path**: `samples/jsonwebtoken_livevaliditywindow/`
**Projects**: `JWTServer.dpr`, `JWTClient.dpr`
**Key Files**:
- `AppControllerU.pas` - JWT with live validity
- `WebModuleUnit1.pas` - JWT configuration
**Description**: JWT with dynamic validity window
**Category**: Security

### 54. jsonwebtoken_roleauth
**(Already listed in Authentication & Authorization section)**

### 55. jsonwebtokenplain
**Path**: `samples/jsonwebtokenplain/`
**Projects**: `jwtplainserver.dpr`, `jwtplainclient.dpr`
**Key Files**:
- `MainWebModuleU.pas` - Plain JWT implementation
- `MyControllerU.pas` - JWT controller
**Description**: Simple JWT implementation without middleware
**Category**: Security

---

## API Documentation (Swagger)

### 56. swagger_api_versioning_primer
**Path**: `samples/swagger_api_versioning_primer/`
**Projects**: `SwaggerAPIVersioning.dpr`, `SwaggerAPIVersioning.dproj`
**Key Files**:
- `MyControllerU.pas` - Versioned API controller
- `WebModuleU.pas` - Swagger setup
**Description**: API versioning with Swagger documentation
**Category**: API Design

### 57. swagger_doc
**Path**: `samples/swagger_doc/`
**Projects**: `SwaggerDocApi.dpr`, `SwaggerDocApi.dproj`
**Key Files**:
- `MyController1U.pas` - First API controller
- `MyController2U.pas` - Second API controller
- `WebModuleMainU.pas` - Swagger configuration
**Description**: Basic Swagger documentation
**Category**: API Documentation

### 58. swagger_doc_extended
**Path**: `samples/swagger_doc_extended/`
**Projects**: `SwaggerDocApiExtended.dpr`
**Key Files**:
- `BaseControllerU.pas` - Base controller
- `ControllersU.pas` - API controllers
- `MyController2U.pas` - Additional controller
**Description**: Extended Swagger documentation features
**Category**: API Documentation

### 59. swagger_primer
**Path**: `samples/swagger_primer/`
**Projects**: `SwaggerPrimer.dpr`, `SwaggerPrimer.dproj`
**Key Files**:
- `MyControllerU.pas` - API controller
- `WebModuleU.pas` - Swagger setup
**Description**: Getting started with Swagger
**Category**: Getting Started

### 60. swagger_ui
**Path**: `samples/swagger_ui/`
**Projects**: `swaggeruiwebserver.dpr`
**Key Files**:
- `MainControllerU.pas` - API controller
- `WebModuleU.pas` - Swagger UI integration
**Description**: Swagger UI integration
**Category**: API Documentation

---

## Session Management

### 61. session_file_based
**Path**: `samples/session_file_based/`
**Projects**: `FileBasedSessionSample.dpr`
**Key Files**:
- `AppControllerU.pas` - Session controller
- `WebModuleUnit1.pas` - File-based session provider
**Description**: File-based session storage
**Category**: State Management

### 62. sessions
**Path**: `samples/sessions/`
**Projects**: `SessionSample.dpr`, `SessionSample.dproj`
**Key Files**:
- `AppControllerU.pas` - Session controller
- `MemoryWebSessionController.pas` - Memory session controller
- `WebModuleUnit1.pas` - Session configuration
**Description**: In-memory session management
**Category**: State Management

---

## Logging

### 63. Logger
**Path**: `samples/Logger/`
**Projects**: `LoggerSample.dpr`, `LoggerSample.dproj`
**Description**: Console-based logging sample
**Category**: Debugging

### 64. LoggerGUI
**Path**: `samples/LoggerGUI/`
**Projects**: `LoggerSampleGUI.dpr`, `LoggerSampleGUI.dproj`
**Description**: GUI-based logging sample
**Category**: Debugging

### 65. custom_logger
**Path**: `samples/custom_logger/`
**Projects**: `CustomLoggerSample.dpr`
**Key Files**:
- `MyControllerU.pas` - Controller with custom logging
- `WebModuleU.pas` - Custom logger setup
**Description**: Custom logger implementation
**Category**: Debugging

### 66. disable_default_logger
**Path**: `samples/disable_default_logger/`
**Projects**: `disable_default_logger.dpr`
**Key Files**:
- `ControllerU.pas` - Controller without default logger
- `WebModuleU.pas` - Logger configuration
**Description**: Disabling default DMVC logger
**Category**: Configuration

### 67. log_filter
**Path**: `samples/log_filter/`
**Projects**: `LogFilterSample.dpr`, `LogFilterSample.dproj`
**Key Files**:
- `ControllerU.pas` - Filtered logging controller
- `WebModuleU.pas` - Log filter setup
**Description**: Filtered logging implementation
**Category**: Debugging

---

## Error Handling

### 68. custom_exception_handling
**Path**: `samples/custom_exception_handling/`
**Projects**: `custom_exception_handling.dpr`
**Key Files**:
- `MyControllerU.pas` - Custom exception handling
- `WebModuleU.pas` - Exception handler setup
**Description**: Custom exception handling in web module
**Category**: Error Management

### 69. custom_exception_handling_using_controller
**Path**: `samples/custom_exception_handling_using_controller/`
**Projects**: `exception_handling_with_controller.dpr`
**Key Files**:
- `MyControllerU.pas` - Controller-based exception handling
- `WebModuleU.pas` - Web module
**Description**: Controller-level exception handling
**Category**: Error Management

---

## Security

### 70. hmac
**Path**: `samples/hmac/`
**Projects**: `hmacsample.dpr`, `hmacsample.dproj`
**Description**: HMAC cryptographic signing
**Category**: Cryptography

### 71. ssl_client
**Path**: `samples/ssl_client/`
**Projects**: `sslclient.dpr`, `sslclient.dproj`
**Description**: SSL/TLS client implementation
**Category**: Encryption

### 72. ssl_server
**Path**: `samples/ssl_server/`
**Projects**: `SSLServer.dpr`, `SSLServer.dproj`
**Key Files**:
- `MyControllerU.pas` - SSL-enabled controller
- `WebModuleUnit1.pas` - SSL configuration
**Description**: SSL/TLS server setup
**Category**: Encryption
**Has README**: Yes

### 73. action_filters
**Path**: `samples/action_filters/`
**Projects**: `ActionFilters.dpr`, `ActionFilters.dproj`
**Key Files**:
- `ActionFiltersControllerU.pas` - Action filter examples
- `WebModuleU.pas` - Web module
**Description**: Action filters for authorization/validation
**Category**: Request Processing

### 74. avoid_mid_air_collisions_sample
**Path**: `samples/avoid_mid_air_collisions_sample/`
**Projects**: `avoid_mid_air_collisions_sample.dpr`
**Key Files**:
- `MainControllerU.pas` - Optimistic locking controller
- `WebModuleU.pas` - Web module
**Description**: Optimistic concurrency control (ETags)
**Category**: Concurrency

---

## Frontend Integration

### 75. angular
**Path**: `samples/angular/`
**Projects**: `AngularSampleServer.dpr`, `AngularSampleServer.dproj`
**Key Files**:
- `CustomersControllerU.pas` - Customer API
- `WebModuleU.pas` - CORS configuration for Angular
**Description**: DMVC backend for Angular frontend
**Category**: SPA Integration
**Has README**: Yes

### 76. react
**Path**: `samples/react/`
**Projects**: `ServerReact.dpr`, `ServerReact.dproj`
**Description**: DMVC backend for React frontend
**Category**: SPA Integration
**Has README**: Yes

---

## Deployment & Hosting

### 77. ISAPI
**Path**: `samples/ISAPI/`
**Projects**: `isapiapp.dpr`, `ConsoleApp.dpr`
**Key Files**:
- `RoutingSampleControllerU.pas` - ISAPI controller
- `WebModuleU.pas` - ISAPI web module
**Description**: IIS ISAPI extension deployment
**Category**: IIS Integration

### 78. apache_module
**Path**: `samples/apache_module/`
**Projects**: `mod_dmvc.dpr`, `mod_dmvc.dproj`
**Description**: Apache web server module
**Category**: Apache Integration

### 79. server_in_dll
**Path**: `samples/server_in_dll/`
**Projects**: `DMVCServerDLL.dpr`, `UsingServerInDLL.dpr`
**Key Files**:
- `REST.WebModule.pas` - DLL web module
**Description**: DMVC server packaged as DLL
**Category**: DLL Hosting

### 80. windows_service
**Path**: `samples/windows_service/`
**Projects**: `dmvcwindowsservice.dpr`, `dmvcwindowsservice.dproj`
**Description**: Windows Service hosting
**Category**: Service Deployment

### 81. services_injection
**Path**: `samples/services_injection/`
**Projects**: `services_injection.dpr`, `services_injection.dproj`
**Key Files**:
- `MainControllerU.pas` - DI controller
- `WebModuleU.pas` - Service registration
**Description**: Service injection and DI container
**Category**: Dependency Injection

---

## Dependency Injection

### 82. servercontainer
**Path**: `samples/servercontainer/`
**Projects**: `ServerContainerBasicDemo.dpr`
**Key Files**:
- `App1MainControllerU.pas` - Controller
- `CustomWebModuleU.pas` - Custom web module
- `WebModule01U.pas` - Additional web module
**Description**: Server container and multi-module setup
**Category**: Architecture

### 83. services_injection
**(Already listed in Deployment & Hosting section)**

---

## Caching & Redis

### 84. outputcachewithredis
**Path**: `samples/outputcachewithredis/`
**Projects**: `OutputCacheWithRedis.dpr`
**Key Files**:
- `BaseControllerU.pas` - Base controller
- `PeopleControllerU.pas` - Cached endpoints
- `WebModuleU.pas` - Redis cache setup
**Description**: Output caching with Redis backend
**Category**: Performance

---

## Performance & Profiling

### 85. profiling
**Path**: `samples/profiling/`
**Projects**: `ProfilingSample.dpr`, `ProfilingSample.dproj`
**Key Files**:
- `MainControllerU.pas` - Profiled controller
- `WebModuleU.pas` - Profiling setup
**Description**: Request profiling and timing
**Category**: Performance Analysis

### 86. profiling_showcase
**Path**: `samples/profiling_showcase/`
**Projects**: `profiler_showcase.dpr`, `profiler_showcase.dproj`
**Description**: Comprehensive profiling features
**Category**: Performance Analysis

---

## Monitoring & Metrics

### 87. prometheus
**Path**: `samples/prometheus/`
**Description**: Prometheus metrics integration
**Category**: Monitoring

---

## File Operations

### 88. file_upload
**Path**: `samples/file_upload/`
**Projects**: `FilesUploadDemo.dpr`, `FilesUploadDemo.dproj`
**Key Files**:
- `FileUploadControllerU.pas` - File upload handler
- `WebModuleUnit1.pas` - Web module
**Description**: Multipart file upload handling
**Category**: File Management

---

## Rendering & Content

### 89. jsonwriterrenders
**Path**: `samples/jsonwriterrenders/`
**Projects**: `jsonwriterrenders.dpr`
**Key Files**:
- `JSONSampleController.pas` - Custom JSON rendering
- `WebModuleU.pas` - Web module
**Description**: Custom JSON rendering techniques
**Category**: Response Formatting

### 90. render_binary_contents
**Path**: `samples/render_binary_contents/`
**Projects**: `render_binary_contents.dpr`
**Key Files**:
- `ControllerU.pas` - Binary content rendering
- `WebModuleU.pas` - Web module
**Description**: Serving binary content (images, PDFs, etc.)
**Category**: Response Formatting

### 91. renders
**Path**: `samples/renders/`
**Projects**: `renders.dpr`, `renders.dproj`
**Key Files**:
- `RenderSampleControllerU.pas` - Various render types
- `WebModuleU.pas` - Web module
**Description**: Multiple rendering formats (JSON, XML, HTML, etc.)
**Category**: Response Formatting

---

## Async Operations

### 92. mvcasync
**Path**: `samples/mvcasync/`
**Projects**: `AsyncTaskSample.dpr`, `AsyncTaskSample.dproj`
**Description**: Asynchronous task execution
**Category**: Concurrency

---

## Advanced Patterns

### 93. avoid_mid_air_collisions_sample
**(Already listed in Security section)**

### 94. concurrency_speed_test
**Path**: `samples/concurrency_speed_test/`
**Projects**: `concurrency_speed_test.dpr`
**Key Files**:
- `MainControllerU.pas` - Concurrency test controller
- `WebModuleU.pas` - Web module
**Description**: Concurrency and load testing
**Category**: Performance Testing

### 95. datapump
**Path**: `samples/datapump/`
**Projects**: `DataPumpSample.dpr`, `DataPumpSample.dproj`
**Description**: Data transformation and pumping utilities
**Category**: Data Processing

### 96. soap_rest
**Path**: `samples/soap_rest/`
**Projects**: `SOAPREST.dpr`, `SOAPREST.dproj`
**Description**: SOAP to REST bridge/adapter
**Category**: Integration

---

## Utilities & Helpers

### 97. bloom_filter
**Path**: `samples/bloom_filter/`
**Projects**: `BloomFilterShowcase.dpr`, `BloomFilterShowcase.dproj`
**Description**: Bloom filter data structure
**Category**: Data Structures

### 98. dotenv_showcase
**Path**: `samples/dotenv_showcase/`
**Projects**: `dotEnv_ShowCase.dpr`, `dotEnv_ShowCase.dproj`
**Description**: Environment variable management (.env files)
**Category**: Configuration

### 99. dotenv_simple
**Path**: `samples/dotenv_simple/`
**Projects**: `dotEnvSimple.dpr`, `dotEnvSimple.dproj`
**Description**: Simple .env file usage
**Category**: Configuration

### 100. higher_order_functions
**Path**: `samples/higher_order_functions/`
**Projects**: `HigherOrderFunctions.dpr`, `HigherOrderFunctions.dproj`
**Description**: Functional programming utilities
**Category**: Functional Programming

### 101. nullable_types_showcase
**Path**: `samples/nullable_types_showcase/`
**Projects**: `NullableTypesShowcase.dpr`
**Description**: Nullable type system showcase
**Category**: Type System

### 102. nullables
**Path**: `samples/nullables/`
**Projects**: `NullablesTest.dpr`, `NullablesTest.dproj`
**Description**: Nullable types basic usage
**Category**: Type System

### 103. objectpool
**Path**: `samples/objectpool/`
**Projects**: `ObjectPoolSample.dpr`, `ObjectPoolSample.dproj`
**Description**: Object pooling pattern
**Category**: Performance

### 104. sqids_showcase
**Path**: `samples/sqids_showcase/`
**Projects**: `sqids_showcase.dpr`, `sqids_showcase.dproj`
**Description**: Sqids (YouTube-like IDs) implementation
**Category**: ID Generation

---

## Complete Examples

### 105. articles_crud_server
**Path**: `samples/articles_crud_server/`
**Projects**: `articles_crud_server.dpr`
**Key Files**:
- `Controllers.Articles.pas` - Article CRUD controller
- `Controllers.Base.pas` - Base controller
- `WebModuleUnit1.pas` - Web module
**Description**: Complete CRUD REST API for articles
**Category**: Reference Implementation

### 106. articles_crud_vcl_client
**Path**: `samples/articles_crud_vcl_client/`
**Projects**: `articles_crud_vcl_client.dpr`
**Description**: VCL client for articles CRUD API
**Category**: Client Implementation

### 107. articles_crud_vcl_client_api_binder
**Path**: `samples/articles_crud_vcl_client_api_binder/`
**Projects**: `articles_crud_vcl_client_api_binder.dpr`
**Description**: VCL client using API binder pattern
**Category**: Client Implementation

### 108. articles_crud_vcl_client_meta
**Path**: `samples/articles_crud_vcl_client_meta/`
**Projects**: `articles_crud_vcl_client_meta.dpr`
**Description**: VCL client with metadata support
**Category**: Client Implementation

### 109. wine_cellar_sample
**Path**: `samples/wine_cellar_sample/`
**Projects**: `WineCellarMobileClient.dpr`, `WineCellarVCLClient.dpr`, `WineCellarMobileClientWithRESTAdapter.dpr`
**Key Files**:
- `MainWebModuleUnit.pas` - Main web module
- `WineCellarAppControllerU.pas` - Wine cellar controller
**Description**: Complete wine cellar management system (mobile + VCL clients)
**Category**: Reference Implementation

---

## Miscellaneous

### 110. RESTClient
**Path**: `samples/RESTClient/`
**Projects**: `RESTClientExample.dpr`, `RESTClientExample.dproj`
**Description**: REST client library usage
**Category**: HTTP Client

### 111. jsondataobjects_serializer
**Path**: `samples/jsondataobjects_serializer/`
**Projects**: `Server.dpr`, `Server.dproj`
**Key Files**:
- `App.WebModule.pas` - Application web module
- `Person.Controller.pas` - Person API with custom serializer
**Description**: JsonDataObjects serialization
**Category**: Serialization

### 112. rql2sql
**Path**: `samples/rql2sql/`
**Projects**: `RQL2SQL.dpr`, `RQL2SQL.dproj`
**Description**: Resource Query Language to SQL conversion
**Category**: Query Language

### 113. webcontextevents
**Path**: `samples/webcontextevents/`
**Projects**: `WebContextEventsSample.dpr`
**Key Files**:
- `MainControllerU.pas` - Web context event handler
- `WebModuleU.pas` - Web module
**Description**: Web context lifecycle events
**Category**: Lifecycle Management

---

## Utility Folders (Non-Samples)

### commons/
**Path**: `samples/commons/`
**Description**: Shared code and utilities used across samples
**Usage**: Referenced by multiple samples

### data/
**Path**: `samples/data/`
**Description**: Sample data files (databases, JSON, etc.)
**Usage**: Test data for various samples

### _/
**Path**: `samples/_/`
**Description**: Deprecated or archived samples
**Usage**: Historical reference only

---

## Statistics by Category

| Category | Count | % of Total |
|----------|-------|------------|
| Middleware | 10 | 9.1% |
| Server-Side Views & HTMX | 9 | 8.2% |
| Database - ActiveRecord & Repository | 7 | 6.4% |
| Core Concepts | 6 | 5.5% |
| WebSockets | 5 | 4.5% |
| API Documentation (Swagger) | 5 | 4.5% |
| Complete Examples | 5 | 4.5% |
| Utilities & Helpers | 7 | 6.4% |
| Logging | 5 | 4.5% |
| Security (all types) | 5 | 4.5% |
| JWT Tokens | 4 | 3.6% |
| Deployment & Hosting | 5 | 4.5% |
| Database - General | 4 | 3.6% |
| Frontend Integration | 2 | 1.8% |
| Others | 31 | 28.2% |

---

## Learning Path Recommendations

### Beginner Path
1. `basicdemo_server` - Understand DMVC basics
2. `routing` - Learn routing patterns
3. `simple_api_using_datasets` - Simple CRUD with TDataSet
4. `swagger_primer` - API documentation basics
5. `sessions` - Session management

### Intermediate Path
1. `simple_api_using_mvcactiverecord` - ActiveRecord pattern
2. `middleware` - Middleware concepts
3. `jsonwebtoken` - JWT authentication
4. `serversideviews_templatepro` - Server-side rendering
5. `articles_crud_server` - Complete REST API

### Advanced Path
1. `middleware_activerecord` - Advanced middleware
2. `repository_showcase` - Repository pattern
3. `services_injection` - Dependency injection
4. `websocket_chat` - Real-time communication
5. `htmx_website_with_templatepro` - Modern interactive UI

### Production Deployment Path
1. `middleware_cors` - CORS configuration
2. `middleware_ratelimit_redis` - Rate limiting
3. `middleware_compression` - Performance optimization
4. `ssl_server` - SSL/TLS setup
5. `windows_service` - Service deployment
6. `prometheus` - Monitoring and metrics

---

## Key Findings

1. **Most comprehensive areas**: Middleware (10 samples), Server-Side Views/HTMX (9 samples)
2. **Modern web focus**: Strong support for HTMX, WebSockets, SSE
3. **Database flexibility**: Multiple patterns (ActiveRecord, Repository, TDataSet, ADO)
4. **Security coverage**: JWT, HMAC, SSL/TLS, CORS, authentication/authorization
5. **Production-ready features**: Rate limiting, compression, caching, monitoring
6. **Multiple deployment options**: Standalone, ISAPI, Apache, Windows Service, DLL
7. **Template engine variety**: Mustache, TemplatePro, WebStencils

---

**Note**: This inventory was generated from the DelmiVCFramework samples directory and represents the official examples as of 2025-12-19. Each sample includes working code, configuration, and in many cases, multiple client/server implementations.
