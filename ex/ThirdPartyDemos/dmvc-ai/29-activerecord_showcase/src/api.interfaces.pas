unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.variants,
  entities;

type
  /// ActiveRecord Showcase API
  // Demonstrates various ORM features and patterns
  IActiveRecordShowcaseApi = interface(IInvokable)
    ['{4C5D6E7F-8A9B-0C1D-2E3F-4A5B6C7D8E9F}']

    // Basic CRUD
    function CreateCustomer(const code, companyName, city, note: RawUtf8;
      rating: Integer): TID;
    function GetCustomer(id: TID): Variant;
    function GetAllCustomers: TVariantDynArray;
    function UpdateCustomer(id: TID; const code, companyName, city,
      note: RawUtf8; rating: Integer): Boolean;
    function DeleteCustomer(id: TID): Boolean;

    // Filtering and searching
    function GetCustomersByCity(const city: RawUtf8): TVariantDynArray;
    function GetCustomersByRating(minRating: Integer): TVariantDynArray;
    function GetActiveCustomers: TVariantDynArray;

    // Aggregation
    function GetCustomerCount: Integer;
    function GetAverageRating: Double;
    function GetCustomerStats: Variant;

    // Batch operations
    function BulkCreateCustomers(const customers: TVariantDynArray): Integer;
    function BulkUpdateRating(minRating, newRating: Integer): Integer;

    // Articles
    function CreateArticle(const code, description: RawUtf8;
      price: Currency; quantity: Integer): TID;
    function GetArticle(id: TID): Variant;
    function GetLowStockArticles(threshold: Integer): TVariantDynArray;

    // Orders (demonstrates relationships)
    function CreateOrder(customerID: TID; totalAmount: Currency;
      const status: RawUtf8): TID;
    function GetOrdersByCustomer(customerID: TID): TVariantDynArray;
    function GetOrderWithCustomer(orderID: TID): Variant;
  end;

implementation

end.
