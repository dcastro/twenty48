# Notes

## Perf checklist

* Changed `Board` representation from list of lists of cells to a flat 16-element unboxed vector
* Specializing `transpose` from `Unbox a => VUM.MVector s a -> ST s ()` to  `VUM.MVector s Cell -> ST s ()` gave us a big perf improvement
* Swapping `(,)` for `Data.Strict.Tuple.Pair` in `Eval.monotonicity` shaved off 15~20%
* Swapping `(,)` for `Data.Strict.Tuple.Pair` in `StateTree` shaved off 5~10%

## Things that were measured but did not speed up execution

* Specializing `sliceRowsM` and `findIndex`
* Using `Vector.Mutable.Unboxed.unsafeWrite`, `unsafeRead`, `unsafeSlice`, `unsafeSwap`
* Swapping `Data.Maybe` for `Data.Strict.Maybe`
