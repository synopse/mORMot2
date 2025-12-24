unit storage;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.core.variants;

const
  STOCK_TITLES: array[0..3] of RawUtf8 = ('IBM', 'AAPL', 'GOOG', 'MSFT');

function GetNextStockUpdate(const LastEventID: Integer;
  out CurrentEventID: Integer): RawUtf8;

implementation

function GetNextStockUpdate(const LastEventID: Integer;
  out CurrentEventID: Integer): RawUtf8;
var
  lIndex: Integer;
  lValue: Double;
  doc: TDocVariantData;
begin
  // Get a different stock than the last one
  lIndex := LastEventID;
  while lIndex = LastEventID do
    lIndex := Random(Length(STOCK_TITLES));

  // Generate random stock price between 500-700
  lValue := 500 + Random(200) + (Random(50) / 100);

  // Create JSON response
  doc.InitFast(2, dvObject);
  doc.AddValue('stock', STOCK_TITLES[lIndex]);
  doc.AddValue('value', lValue);

  Result := doc.ToJson;
  CurrentEventID := LastEventID + 1;
end;

end.
