# Author's questions on the `SetOperationResult` / `RaiseIfExtractFailed` fix

While reviewing Change 2 in [`mormot_7zip_changes.md`](mormot_7zip_changes.md)
(the fix that stops extraction failures being silently ignored), the author
raised two questions about the design. Both are good questions — here are the
answers, and what came out of them.

---

## Q1 — Why not put `RaiseIfExtractFailed()` as part of `E7Zip.CheckOk`?

Because the two check **two independent signals**, and `CheckOk` is the wrong
home for the second one.

- `CheckOk(Caller, Context, Res)` validates the **HRESULT** returned by *any*
  7z.dll call — `OpenFile`, `GetNumberOfItems`, `SetProperty`, `SaveToStream`,
  `Extract`, and so on. `RaiseIfExtractFailed` checks a completely different
  thing: the **recorded per-item operation result** (`fExtractOpResult`). The
  whole point of the bug is that `fInArchive.Extract` returns `S_OK` *while
  individual entries failed their CRC*. The HRESULT (transport-level "did the
  API call succeed?") and the operation result (content-level "did each file
  decode correctly?") are orthogonal. Folding one into the other conflates them.

- `CheckOk` is a **class (static) procedure** on `E7Zip`, and it only receives
  `Caller` as an untyped `TObject`. It has no access to `fExtractOpResult`. To
  read that field it would have to `is`/`as`-cast `Caller` down to `T7zReader` —
  but `T7zWriter` also calls `CheckOk` and has no such field. That would couple a
  generic exception helper to one specific class.

- `fExtractOpResult` is only meaningful inside the *reset → extract → check*
  window. `CheckOk` is called pervasively; you do not want every unrelated
  `CheckOk` (during `OpenFile`, `SetProperty`, …) consulting a stateful field
  that may still hold a value from an earlier extraction.

**Verdict:** merging into `CheckOk` would be worse. If the goal is simply less
repetition at the call sites, a small dedicated `CheckExtract` helper that runs
*both* checks in the right order (HRESULT first, then the operation result) would
be a reasonable style tweak — but that is not the same as putting it in the
general-purpose `CheckOk`.

---

## Q2 — Why not reset `fExtractOpResult` in `RaiseIfExtractFailed()`?

Because **reset-before-use is more robust than reset-after-use**, and it keeps
the helper a pure query.

- The current code resets `fExtractOpResult := eorOK` at the **start** of each
  extract, right before the DLL call. That guarantees a clean slate no matter how
  the *previous* operation ended — including if it threw before reaching its
  check, or went through a path that does not call `RaiseIfExtractFailed` at all.

- `RaiseIfExtractFailed`'s job is "check and raise". If it also reset the field,
  it would become non-idempotent (calling it twice would behave differently the
  second time) and it would mix a query with a mutation. As a pure check it is
  safe to call repeatedly, and after it raises the field still reflects what
  happened, for anyone inspecting state.

- Reset-on-exit is fragile: on the failure path the helper *raises*, so any reset
  has to be carefully ordered around the raise, and any path that never calls the
  helper would never get reset.

**Verdict:** keep the reset where it is (before the call). Resetting inside
`RaiseIfExtractFailed` would be less robust and would overload the helper with a
second responsibility.

---

## What the questions surfaced

Thinking through *where* the check belongs (Q1) exposed a real gap in the
original fix: only the two **to-disk** overloads (`Extract(item, path)` and
`ExtractAll(path)`) reset and checked `fExtractOpResult`. The
**stream / callback** overloads did not — so a wrong ZipCrypto password extracted
into a stream (via `Extract(item, Stream)`, and therefore
`Extract(zipname): RawByteString` and `Extract(zipname, Stream): boolean`) still
returned garbage *bytes* silently, exactly the bug the change set out to kill,
just through a different overload.

That gap is now closed: `Extract(item, Stream)`, `Extract(items, callback)` and
`ExtractAll(callback)` all reset-before and check-after as well, so **every**
extract path raises `E7Zip` on a recorded failure. (The `boolean` /
`RawByteString` helpers still swallow the exception by design, so they now return
`False` / `''` instead of success-with-garbage.)

Notably, this was cheap *because* of the answer to Q2: since the reset lives at
the start of each overload rather than inside the raise helper, extending the
guard to three more entry points was a mechanical two-line addition each, with no
risk of one path's stale state leaking into another.

See [`mormot_7zip_changes.md`](mormot_7zip_changes.md) for the full description,
and `test/win7zip/win7ziptest.dpr` for a standalone console program that
exercises all of this (14 checks, including the stream-path cases).
