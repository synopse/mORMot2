
unit tag_mappers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.os,
  tag,
  tag_dtos;

function OrmToTagViewDTO(aTag: TTag): TTagViewDTO;

function CreateTagDTOToOrm(const aDto: TTagCreateDTO; aTag: TTag;
  out aError: RawUtf8): boolean;

function ApplyUpdateTagDTO(const aDto: TTagUpdateDTO; aTag: TTag;
  out aError: RawUtf8): boolean;

/// Normalize a tag name (trim + lowercase) for unique-key comparisons.
function NormalizeTagName(const aName: RawUtf8): RawUtf8;

implementation

function OrmToTagViewDTO(aTag: TTag): TTagViewDTO;
begin
  Result.ID := aTag.ID;
  Result.Name := aTag.Name;
  Result.Color := aTag.Color;
  Result.CreatedAt := aTag.CreatedAt;
end;

function NormalizeTagName(const aName: RawUtf8): RawUtf8;
begin
  Result := LowerCase(Trim(aName));
end;

function ResolveColor(const aColor: RawUtf8): RawUtf8;
begin
  if (aColor = '') or not IsValidColor(aColor) then
    Result := GetDefaultColor
  else
    Result := aColor;
end;

function CreateTagDTOToOrm(const aDto: TTagCreateDTO; aTag: TTag;
  out aError: RawUtf8): boolean;
begin
  aError := '';
  if NormalizeTagName(aDto.Name) = '' then
  begin
    aError := 'Tag name cannot be empty';
    Result := false;
    exit;
  end;
  aTag.Name := NormalizeTagName(aDto.Name);
  aTag.Color := ResolveColor(aDto.Color);
  aTag.CreatedAt := NowUtc;
  aTag.SchemaVersion := TAG_CURRENT_VERSION;
  Result := true;
end;

function ApplyUpdateTagDTO(const aDto: TTagUpdateDTO; aTag: TTag;
  out aError: RawUtf8): boolean;
begin
  aError := '';
  if NormalizeTagName(aDto.Name) = '' then
  begin
    aError := 'Tag name cannot be empty';
    Result := false;
    exit;
  end;
  aTag.Name := NormalizeTagName(aDto.Name);
  aTag.Color := ResolveColor(aDto.Color);
  Result := true;
end;

end.
