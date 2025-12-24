# Bloom Filter Sample

## Overview

This sample demonstrates the use of mORMot2's `TSynBloomFilter` class from the `mormot.core.search` unit.

## What is a Bloom Filter?

A **Bloom Filter** is a space-efficient probabilistic data structure used to test whether an element is a member of a set.

### Key Characteristics:
- **False positives** are possible, but **false negatives** are not
- Elements can be added but not removed
- Memory use is very low compared to storing all values
- ~10 bits per element for 1% false positive probability
- Thread-safe implementation in mORMot2

### Use Cases:
- Caching: Avoid expensive disk/network access if item definitely doesn't exist
- Pre-filtering: Check before expensive database queries
- Duplicate detection: Fast membership testing for large datasets
- Network protocols: Reduce bandwidth by filtering unnecessary requests

## What This Sample Demonstrates

1. **Basic Operations**
   - Creating a bloom filter with specified size and false positive rate
   - Inserting items (`Insert()`)
   - Testing membership (`MayExist()`)
   - Understanding false positives (never false negatives)

2. **Serialization**
   - Saving bloom filter state to binary (`SaveTo()`)
   - Loading from binary (`LoadFrom()`)
   - Preserving filter state across program restarts

3. **Performance**
   - Comparing bloom filter lookups vs. linear search
   - Demonstrating memory efficiency (10 bits/item vs. 20+ bytes/item)
   - Showing O(k) lookup time (where k = hash functions, typically 7)

4. **Configuration**
   - Impact of false positive rate on memory usage
   - Tradeoff between memory and accuracy
   - Hash function count optimization

## Building and Running

### Prerequisites
- RAD Studio 12 (Delphi 29.0) or compatible
- mORMot2 framework source code

### Compilation

Using delphi-compiler.exe:
```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe BloomFilterSample.dproj --config=Release
```

Or from Delphi IDE:
1. Open `BloomFilterSample.dproj`
2. Build → Build BloomFilterSample
3. Run → Run

### Expected Output

The sample runs four demonstrations:

1. **Basic Bloom Filter Demo**
   - Inserts 10 fruit names
   - Tests 5 existing items (all should return true)
   - Tests 5 non-existing items (may have false positives)
   - Shows actual false positive rate

2. **Serialization Demo**
   - Creates filter with 100 items
   - Serializes to binary
   - Deserializes into new filter
   - Verifies items still present

3. **Performance Demo**
   - Creates filter with 10,000 items
   - Compares 1,000 lookups: bloom filter vs. linear search
   - Shows speedup and memory savings

4. **False Positive Rate Comparison**
   - Creates three filters: 1%, 5%, and 10% FP rates
   - Shows memory usage for each
   - Verifies actual FP rate matches configuration

## Key API Usage

```pascal
uses
  mormot.core.search;

var
  bloom: TSynBloomFilter;

// Create filter for 1000 items with 1% false positive rate
bloom := TSynBloomFilter.Create(1000, 1.0);
try
  // Add items
  bloom.Insert('item1');
  bloom.Insert('item2');

  // Check membership
  if bloom.MayExist('item1') then
    WriteLn('item1 probably exists'); // True

  if bloom.MayExist('item999') then
    WriteLn('False positive!'); // Possible but unlikely

  // Serialize
  saved := bloom.SaveTo;

  // Deserialize
  bloom2 := TSynBloomFilter.Create(saved);

finally
  bloom.Free;
end;
```

## Memory Efficiency Example

For 10,000 items:
- **Regular array**: ~200 KB (20 bytes per string)
- **Bloom filter (1% FP)**: ~11.5 KB (9.6 bits per item)
- **Savings**: ~95% memory reduction

## Performance Characteristics

- **Insert**: O(k) where k = number of hash functions (typically 7)
- **Lookup**: O(k) - constant time, extremely fast
- **Memory**: O(n × m) where n = items, m = bits per item (~10 for 1% FP)
- **Thread-safe**: Uses TRWLock for concurrent reads

## References

- [Wikipedia: Bloom Filter](https://en.wikipedia.org/wiki/Bloom_filter)
- mORMot2 documentation: `mormot.core.search` unit
- Original paper: Burton H. Bloom (1970)

## mORMot2 Features

This sample is part of the DMVC-to-mORMot2 sample migration project, demonstrating:
- Cross-platform console application patterns
- Use of mORMot2 core units
- Thread-safe data structures
- Binary serialization
- High-performance algorithms

## License

This sample code is part of the mORMot2 framework, licensed under MPL/GPL/LGPL tri-license.
