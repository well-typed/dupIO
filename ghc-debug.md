# Notes for examining conduit leaks using `ghc-debug`

### Source CAF



### Source non-CAF

Open `Source-leak.snapshot` in `ghc-debug-brick`. The snapshot is taken just
before the memory check in test `Test.DupIO.Conduit.Source.test_withoutDupIO`.
Find retainers of `Yield` constructors, and examine the first `Yield` in the
resulting list. You will see that it is retained by many other `Yield`s up to
the first `Yield`, which is then retained by the following closure chain:
- `runCoduit c` in `test_withoutDupIO`, retained by
- `(runConduit c <* checkMem ...)` in `test_withoutDupIO`, retained by
- the `go` in `retry`, which is retained by itself

This makes it clear that the chain of `Yield`s is retained by the `retry`
exception handler.
