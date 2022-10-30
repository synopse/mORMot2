# Thomas Examples

## Folder Content

This folder hosts some Third-Party Examples of the *mORMot* Open Source framework, version 2.

This folder contains sub-folders with some example code committed by Thomas, alias `tbo`, a third party mORMot user.
Each demo is published as pedagogical material.

## Source

The original articles were posted, in German, [in the Delphi Praxis forum](https://www.delphipraxis.net). The documentation was translated using online translator into English.

## Abstract

### 01-ORM-DocVariant

[The example](./01-ORM-DocVariant) shows a very simplified image database with the following technical details:
- An embedded SQLite database that can be optionally AES encrypted.
- Complete tasks such as storing and reading records with the built-in ORM.
- A full text search using the Title and Comment fields.
- A field for meta data that can contain different fields for each record and can be queried via SQL.
- Acceleration in saving and reading graphic formats JPEG, PNG, GIF, TIFF.

The interface is kept very simple and includes only a few functions. The source code is written in such a way that it invites you to try it out on your own.

### 02-Zip-AES-Pictures

[With the example code](./02-Zip-AES-Pictures) you get:
- A ZIP file as data storage, which can optionally store entries encrypted with AES.
- Connection of a progress indicator with the help of a mediator.
- Introducing a function that can AES encrypt and decrypt a file.
- Acceleration in saving and reading graphic formats JPEG, PNG, GIF, TIFF.
- And some little things more...

### 03-HttpServer-MethodServices

[With this example code](./03-HttpServer-MethodServices) you get:
- An HTTP server for HTTP/1.0/1.1 requests.
- Authentication of the user on the server and creation of a user-specific directory.
- Login with the client to the server and exchange content list and image documents.
- Connection of a progress indicator with the help of a mediator.
- Acceleration in saving and reading graphic formats JPEG, PNG, GIF, TIFF.
- And some little things more...

### 04-HttpServer-InterfaceServices

[With this example](./04-HttpServer-InterfaceServices) you get:
- An HTTP server for HTTP/1.0/1.1 requests.
- Authentication of the user on the server and creation of a user-specific directory.
- Login with the client to the server and exchange content list and text documents.
- A simple Markdown editor with preview of HTML output in the browser window.
- And some little things more...

### 05-WebMustache

[With this application](./05-WebMustache) you get:
- A simple mustache editor (TMemo/TSynEdit component) with auto-completion for common HTML tags.
- An integrated HTTP server for HTTP/1.0, HTTP/1.1, HTTPS with self-signed or own certificate.
- The Edge web browser for viewing HTML pages with undockable stand-alone window.
- Direct linking of images, CSS, JavaScript from ZIP files into the HTML page.
- And some little things more...

