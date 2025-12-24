unit api.impl;

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.server,
  mormot.soa.server,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  api.interfaces;

type
  /// Advanced session API implementation
  TSessionApi = class(TInjectableObjectRest, ISessionApi)
  public
    function SetValue(const key, value: RawUtf8): RawUtf8;
    function GetValue(const key: RawUtf8): RawUtf8;
    function ListKeys: TRawUtf8DynArray;
    function DeleteValue(const key: RawUtf8): boolean;
    function ClearSession: boolean;
    function SessionInfo: RawUtf8;
    function ListActiveSessions: RawUtf8;
    function IncrementCounter(const key: RawUtf8): integer;
  end;

  /// Shopping cart item
  TCartItem = packed record
    ItemId: RawUtf8;
    Quantity: integer;
    Price: currency;
  end;
  TCartItemDynArray = array of TCartItem;

  /// Per-session shopping cart implementation
  /// This demonstrates sicPerSession - one instance per user session
  TCartApi = class(TInjectableObjectRest, ICartApi)
  private
    fItems: TCartItemDynArray;
    function FindItem(const itemId: RawUtf8): integer;
    function GetItemPrice(const itemId: RawUtf8): currency;
  public
    function AddItem(const itemId: RawUtf8; quantity: integer): RawUtf8;
    function RemoveItem(const itemId: RawUtf8): boolean;
    function GetCart: RawUtf8;
    function ClearCart: boolean;
    function GetTotal: currency;
  end;

implementation

uses
  mormot.rest.core,
  SysUtils;

{ TSessionApi }

function TSessionApi.SetValue(const key, value: RawUtf8): RawUtf8;
var
  user: TAuthUser;
  doc: TDocVariantData;
begin
  user := Server.SessionGetUser(ServiceRunningContext^.Request.Session);
  if user = nil then
  begin
    Result := JsonEncode(['error', 'No session found']);
    exit;
  end;

  // Parse existing session data as JSON object
  doc.InitJson(user.Data, JSON_FAST);

  // Set the key
  doc.AddValue(key, variant(value));

  // Save back to session
  user.Data := doc.ToJson;

  Result := JsonEncode(['success', true, 'key', key, 'value', value]);
end;

function TSessionApi.GetValue(const key: RawUtf8): RawUtf8;
var
  user: TAuthUser;
  doc: TDocVariantData;
  idx: integer;
begin
  user := Server.SessionGetUser(ServiceRunningContext^.Request.Session);
  if user = nil then
  begin
    Result := JsonEncode(['error', 'No session found']);
    exit;
  end;

  doc.InitJson(user.Data, JSON_FAST);

  idx := doc.GetValueIndex(key);
  if idx >= 0 then
    Result := JsonEncode(['key', key, 'value', doc.Values[idx]])
  else
    Result := JsonEncode(['error', 'Key not found', 'key', key]);
end;

function TSessionApi.ListKeys: TRawUtf8DynArray;
var
  user: TAuthUser;
  doc: TDocVariantData;
  i: integer;
begin
  user := Server.SessionGetUser(ServiceRunningContext^.Request.Session);
  if user = nil then
    exit;

  doc.InitJson(user.Data, JSON_FAST);

  SetLength(Result, doc.Count);
  for i := 0 to doc.Count - 1 do
    Result[i] := doc.Names[i];
end;

function TSessionApi.DeleteValue(const key: RawUtf8): boolean;
var
  user: TAuthUser;
  doc: TDocVariantData;
begin
  Result := false;
  user := Server.SessionGetUser(ServiceRunningContext^.Request.Session);
  if user = nil then
    exit;

  doc.InitJson(user.Data, JSON_FAST);
  Result := doc.Delete(key);

  if Result then
    user.Data := doc.ToJson;
end;

function TSessionApi.ClearSession: boolean;
var
  user: TAuthUser;
begin
  Result := false;
  user := Server.SessionGetUser(ServiceRunningContext^.Request.Session);
  if user = nil then
    exit;

  user.Data := '{}';
  Result := true;
end;

function TSessionApi.SessionInfo: RawUtf8;
var
  user: TAuthUser;
  sessionId: cardinal;
  info: TDocVariantData;
begin
  sessionId := ServiceRunningContext^.Request.Session;
  user := Server.SessionGetUser(sessionId);
  if user = nil then
  begin
    Result := JsonEncode(['error', 'No session found']);
    exit;
  end;

  info.InitFast;
  info.AddValue('session_id', sessionId);
  info.AddValue('user', user.LogonName);
  info.AddValue('display_name', user.DisplayName);
  info.AddValue('data', user.Data);

  Result := info.ToJson;
end;

function TSessionApi.ListActiveSessions: RawUtf8;
begin
  // Note: mORMot2 doesn't expose SessionGetAll directly
  // This would require custom implementation or access to Server.fSessions
  // For now, return a stub message
  Result := JsonEncode(['message', 'Session listing not available in this implementation']);
end;

function TSessionApi.IncrementCounter(const key: RawUtf8): integer;
var
  user: TAuthUser;
  doc: TDocVariantData;
  currentValue: variant;
begin
  Result := 0;
  user := Server.SessionGetUser(ServiceRunningContext^.Request.Session);
  if user = nil then
    exit;

  doc.InitJson(user.Data, JSON_FAST);

  // Get current value or start from 0
  currentValue := doc.GetValueOrDefault(key, 0);
  Result := VariantToIntegerDef(currentValue, 0);

  // Increment
  Inc(Result);

  // Save back
  doc.AddValue(key, Result);
  user.Data := doc.ToJson;
end;

{ TCartApi }

function TCartApi.FindItem(const itemId: RawUtf8): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to high(fItems) do
    if fItems[i].ItemId = itemId then
    begin
      Result := i;
      break;
    end;
end;

function TCartApi.GetItemPrice(const itemId: RawUtf8): currency;
begin
  // Simple price lookup (in real app, would query database)
  if itemId = 'item1' then
    Result := 10.50
  else if itemId = 'item2' then
    Result := 25.00
  else if itemId = 'item3' then
    Result := 15.75
  else
    Result := 0;
end;

function TCartApi.AddItem(const itemId: RawUtf8; quantity: integer): RawUtf8;
var
  idx: integer;
  item: TCartItem;
begin
  idx := FindItem(itemId);

  if idx >= 0 then
  begin
    // Update quantity
    fItems[idx].Quantity := fItems[idx].Quantity + quantity;
  end
  else
  begin
    // Add new item
    item.ItemId := itemId;
    item.Quantity := quantity;
    item.Price := GetItemPrice(itemId);

    SetLength(fItems, length(fItems) + 1);
    fItems[high(fItems)] := item;
  end;

  Result := JsonEncode(['success', true, 'item_id', itemId, 'quantity', quantity]);
end;

function TCartApi.RemoveItem(const itemId: RawUtf8): boolean;
var
  idx: integer;
begin
  idx := FindItem(itemId);
  Result := idx >= 0;

  if Result then
  begin
    // Remove item by shifting array
    if idx < high(fItems) then
      Move(fItems[idx + 1], fItems[idx], (high(fItems) - idx) * SizeOf(TCartItem));
    SetLength(fItems, length(fItems) - 1);
  end;
end;

function TCartApi.GetCart: RawUtf8;
var
  i: integer;
  cart: TDocVariantData;
  items: TDocVariantData;
  item: TDocVariantData;
begin
  items.InitFast(length(fItems), dvArray);

  for i := 0 to high(fItems) do
  begin
    item.InitFast;
    item.AddValue('item_id', fItems[i].ItemId);
    item.AddValue('quantity', fItems[i].Quantity);
    item.AddValue('price', fItems[i].Price);
    item.AddValue('subtotal', fItems[i].Price * fItems[i].Quantity);
    items.AddItem(variant(item));
  end;

  cart.InitFast;
  cart.AddValue('items', variant(items));
  cart.AddValue('total', GetTotal);
  cart.AddValue('item_count', length(fItems));

  Result := cart.ToJson;
end;

function TCartApi.ClearCart: boolean;
begin
  SetLength(fItems, 0);
  Result := true;
end;

function TCartApi.GetTotal: currency;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to high(fItems) do
    Result := Result + (fItems[i].Price * fItems[i].Quantity);
end;

end.
