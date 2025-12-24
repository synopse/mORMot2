unit datapump;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.perf,
  mormot.orm.core,
  mormot.orm.base,
  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3.static,
  entities;

type
  /// Main data pump class - handles ETL operations
  // Similar to DMVC's ActiveRecord datapump but using mORMot2 ORM
  TDataPump = class
  private
    procedure CreateSampleCustomers(const aRest: IRestOrm);
    procedure CreateSampleOrders(const aRest: IRestOrm);
  public
    /// Create source database with sample data
    procedure CreateSourceDatabase(const aFileName: RawUtf8);

    /// Create empty destination database
    procedure CreateDestinationDatabase(const aFileName: RawUtf8);

    /// Pump data from source to destination
    // Demonstrates Extract, Transform, Load pattern
    procedure PumpData(const aSource, aDest: RawUtf8);

    /// Demonstrate high-performance batch insert
    procedure DemonstrateBatchInsert(const aFileName: RawUtf8);

    /// Get record count for verification
    function GetRecordCount(const aFileName: RawUtf8; const aTable: RawUtf8): integer;
  end;

implementation

{ TDataPump }

procedure TDataPump.CreateSourceDatabase(const aFileName: RawUtf8);
var
  model: TOrmModel;
  rest: TRestServerDB;
begin
  DeleteFile(Utf8ToString(aFileName));

  model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
  try
    rest := TRestServerDB.Create(model, aFileName);
    try
      rest.Server.CreateMissingTables;

      WriteLn('  Generating sample customers...');
      CreateSampleCustomers(rest.ORM);

      WriteLn('  Generating sample orders...');
      CreateSampleOrders(rest.ORM);
    finally
      rest.Free;
    end;
  finally
    model.Free;
  end;
end;

procedure TDataPump.CreateDestinationDatabase(const aFileName: RawUtf8);
var
  model: TOrmModel;
  rest: TRestServerDB;
begin
  DeleteFile(Utf8ToString(aFileName));

  model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
  try
    rest := TRestServerDB.Create(model, aFileName);
    try
      rest.Server.CreateMissingTables;
    finally
      rest.Free;
    end;
  finally
    model.Free;
  end;
end;

procedure TDataPump.PumpData(const aSource, aDest: RawUtf8);
var
  sourceModel, destModel: TOrmModel;
  sourceRest, destRest: TRestServerDB;
  customers: TObjectList;
  orders: TObjectList;
  customer: TOrmCustomer;
  order: TOrmOrder;
  i, j: integer;
  timer: TPrecisionTimer;
  customerCount, orderCount: integer;
  batch: TRestBatch;
  oldID, newID: TID;
  idMap: array of record
    OldID: TID;
    NewID: TID;
  end;
begin
  WriteLn('  Opening source database...');
  sourceModel := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
  try
    sourceRest := TRestServerDB.Create(sourceModel, aSource);
    try
      WriteLn('  Opening destination database...');
      destModel := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
      try
        destRest := TRestServerDB.Create(destModel, aDest);
        try
          timer.Start;

          // Extract customers from source
          WriteLn('  Extracting customers...');
          customers := sourceRest.ORM.RetrieveList(TOrmCustomer, '', []);
          try
            customerCount := customers.Count;
            WriteLn('    Found ', customerCount, ' customers');

            // Load customers to destination using batch operations
            WriteLn('  Loading customers to destination (using batch)...');
            batch := TRestBatch.Create(destRest.ORM, TOrmCustomer, 1000);
            try
              SetLength(idMap, customerCount);

              for i := 0 to customers.Count - 1 do
              begin
                customer := TOrmCustomer(customers.Items[i]);
                oldID := customer.ID;

                // Transform: could modify data here
                // For example: customer.City := UpperCase(customer.City);

                // Add to batch
                newID := batch.Add(customer, true, {ForceID=}false);
                idMap[i].OldID := oldID;
                idMap[i].NewID := newID;
              end;

              // Execute batch
              if destRest.ORM.BatchSend(batch) = HTTP_SUCCESS then
                WriteLn('    Successfully inserted ', customerCount, ' customers')
              else
                raise Exception.Create('Failed to insert customers');
            finally
              batch.Free;
            end;
          finally
            customers.Free;
          end;

          // Extract orders from source
          WriteLn('  Extracting orders...');
          orders := sourceRest.ORM.RetrieveList(TOrmOrder, '', []);
          try
            orderCount := orders.Count;
            WriteLn('    Found ', orderCount, ' orders');

            // Load orders to destination with ID mapping
            WriteLn('  Loading orders to destination (with FK mapping)...');
            batch := TRestBatch.Create(destRest.ORM, TOrmOrder, 1000);
            try
              for i := 0 to orders.Count - 1 do
              begin
                order := TOrmOrder(orders.Items[i]);

                // Transform: map old customer ID to new customer ID
                oldID := order.CustomerID;
                for j := 0 to High(idMap) do
                  if idMap[j].OldID = oldID then
                  begin
                    order.CustomerID := idMap[j].NewID;
                    Break;
                  end;

                // Could apply other transformations
                // For example: if order.Amount > 1000 then order.Status := 'VIP';

                batch.Add(order, true, {ForceID=}false);
              end;

              // Execute batch
              if destRest.ORM.BatchSend(batch) = HTTP_SUCCESS then
                WriteLn('    Successfully inserted ', orderCount, ' orders')
              else
                raise Exception.Create('Failed to insert orders');
            finally
              batch.Free;
            end;
          finally
            orders.Free;
          end;

          WriteLn('  Data pump completed in ', timer.Stop, ' (',
                  MicroSecToString(timer.TimeInMicroSec), ')');
          WriteLn('  Total records: ', customerCount + orderCount);
          WriteLn('  Throughput: ', ((customerCount + orderCount) * 1000000) div
                  (timer.TimeInMicroSec + 1), ' records/sec');
        finally
          destRest.Free;
        end;
      finally
        destModel.Free;
      end;
    finally
      sourceRest.Free;
    end;
  finally
    sourceModel.Free;
  end;
end;

procedure TDataPump.DemonstrateBatchInsert(const aFileName: RawUtf8);
var
  model: TOrmModel;
  rest: TRestServerDB;
  batch: TRestBatch;
  customer: TOrmCustomer;
  i: integer;
  timer: TPrecisionTimer;
  count: integer;
begin
  WriteLn('  Opening destination database...');
  model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
  try
    rest := TRestServerDB.Create(model, aFileName);
    try
      count := 1000;
      WriteLn('  Inserting ', count, ' additional customers using batch...');

      timer.Start;

      batch := TRestBatch.Create(rest.ORM, TOrmCustomer, 1000);
      try
        for i := 1 to count do
        begin
          customer := TOrmCustomer.Create;
          customer.Code := FormatUtf8('BATCH%', [i]);
          customer.CompanyName := FormatUtf8('Batch Company %', [i]);
          customer.City := 'BatchCity';
          customer.LastContact := NowUtc;
          customer.Rating := (i mod 5) + 1;
          customer.Note := 'Created via batch insert demonstration';

          batch.Add(customer, true);
        end;

        if rest.ORM.BatchSend(batch) = HTTP_SUCCESS then
          WriteLn('    Successfully inserted ', count, ' customers in ',
                  timer.Stop, ' (', MicroSecToString(timer.TimeInMicroSec), ')')
        else
          raise Exception.Create('Batch insert failed');
      finally
        batch.Free;
      end;

      WriteLn('  Throughput: ', (count * 1000000) div (timer.TimeInMicroSec + 1),
              ' records/sec');
    finally
      rest.Free;
    end;
  finally
    model.Free;
  end;
end;

function TDataPump.GetRecordCount(const aFileName: RawUtf8;
  const aTable: RawUtf8): integer;
var
  model: TOrmModel;
  rest: TRestServerDB;
  ormClass: TOrmClass;
begin
  Result := 0;

  if aTable = 'Customers' then
    ormClass := TOrmCustomer
  else if aTable = 'Orders' then
    ormClass := TOrmOrder
  else
    Exit;

  model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
  try
    rest := TRestServerDB.Create(model, aFileName);
    try
      Result := rest.ORM.TableRowCount(ormClass);
    finally
      rest.Free;
    end;
  finally
    model.Free;
  end;
end;

procedure TDataPump.CreateSampleCustomers(const aRest: IRestOrm);
const
  CITIES: array[0..9] of RawUtf8 = (
    'New York', 'London', 'Paris', 'Berlin', 'Tokyo',
    'Madrid', 'Rome', 'Amsterdam', 'Vienna', 'Prague'
  );
  COMPANIES: array[0..9] of RawUtf8 = (
    'Acme Corporation', 'Global Industries', 'Tech Solutions',
    'Innovation Labs', 'Digital Ventures', 'Smart Systems',
    'Future Tech', 'Quantum Corp', 'Synergy Group', 'Prime Industries'
  );
var
  customer: TOrmCustomer;
  i: integer;
  batch: TRestBatch;
begin
  batch := TRestBatch.Create(aRest, TOrmCustomer, 1000);
  try
    for i := 1 to 100 do
    begin
      customer := TOrmCustomer.Create;
      customer.Code := FormatUtf8('CUST%', [i]);
      customer.CompanyName := COMPANIES[i mod 10];
      customer.City := CITIES[i mod 10];
      customer.LastContact := NowUtc - Random(365);
      customer.Rating := (i mod 5) + 1;
      customer.Note := FormatUtf8('Sample customer #%', [i]);

      batch.Add(customer, true);
    end;

    if aRest.BatchSend(batch) <> HTTP_SUCCESS then
      raise Exception.Create('Failed to create sample customers');
  finally
    batch.Free;
  end;
end;

procedure TDataPump.CreateSampleOrders(const aRest: IRestOrm);
const
  STATUSES: array[0..4] of RawUtf8 = (
    'Pending', 'Processing', 'Shipped', 'Delivered', 'Cancelled'
  );
var
  order: TOrmOrder;
  i: integer;
  batch: TRestBatch;
  customerID: TID;
begin
  batch := TRestBatch.Create(aRest, TOrmOrder, 1000);
  try
    for i := 1 to 500 do
    begin
      // Reference customers 1-100
      customerID := (i mod 100) + 1;

      order := TOrmOrder.Create;
      order.CustomerID := customerID;
      order.OrderDate := NowUtc - Random(180);
      order.OrderNumber := FormatUtf8('ORD-%', [i]);
      order.Amount := Random(10000) / 10.0;
      order.Status := STATUSES[i mod 5];

      batch.Add(order, true);
    end;

    if aRest.BatchSend(batch) <> HTTP_SUCCESS then
      raise Exception.Create('Failed to create sample orders');
  finally
    batch.Free;
  end;
end;

end.
