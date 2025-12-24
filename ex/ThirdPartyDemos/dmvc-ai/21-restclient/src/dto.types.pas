unit dto.types;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.json;

type
  /// DTO for JSONPlaceholder Post entity
  TPostDto = packed record
    UserId: Integer;
    Id: Integer;
    Title: RawUtf8;
    Body: RawUtf8;
  end;
  PPostDto = ^TPostDto;
  TPostDtos = array of TPostDto;

  /// DTO for JSONPlaceholder User entity
  TUserDto = packed record
    Id: Integer;
    Name: RawUtf8;
    Username: RawUtf8;
    Email: RawUtf8;
    Phone: RawUtf8;
    Website: RawUtf8;
  end;
  PUserDto = ^TUserDto;
  TUserDtos = array of TUserDto;

  /// DTO for JSONPlaceholder Comment entity
  TCommentDto = packed record
    PostId: Integer;
    Id: Integer;
    Name: RawUtf8;
    Email: RawUtf8;
    Body: RawUtf8;
  end;
  PCommentDto = ^TCommentDto;
  TCommentDtos = array of TCommentDto;

  /// Response envelope for HTTPBin /headers endpoint
  THttpBinHeaders = packed record
    Accept: RawUtf8;
    Host: RawUtf8;
    UserAgent: RawUtf8;
    CustomHeader: RawUtf8;
  end;

  THttpBinHeadersResponse = packed record
    Headers: THttpBinHeaders;
  end;

  /// Response for HTTPBin /ip endpoint
  THttpBinIpResponse = packed record
    Origin: RawUtf8;
  end;

  /// Response for HTTPBin /bearer endpoint
  THttpBinBearerResponse = packed record
    Authenticated: Boolean;
    Token: RawUtf8;
  end;

implementation

// No initialization needed - mORMot2 handles record JSON serialization automatically

end.
