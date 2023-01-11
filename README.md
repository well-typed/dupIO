# `dupIO`

See (Haddock) documentation of `dupIO` for rationale and discussion.

## Test suite

The test suite is written in a very particular way, to try and ensure that it
is robust and specific about what happens when:

* We run with `-fno-full-laziness` and `-fno-state-hack`, ensuring that ghc
  won't move `let` bindings out or in, respectively.

* We redefine `IO a` as

  ```haskell
  type IO a = State# RealWorld -> (# State# RealWorld, a #)
  ```

  This allows us to distinguish between an IO action which allocates something
  when it is called (`\w -> let x = ..`) versus an IO action which references
  something that has already been allocated (`let x = .. in \w -> ..`).

* The tests follow the same general structure outlined in the
  [Sharing, Space Leaks, and Conduit and friends](https://well-typed.com/blog/2016/09/sharing-conduit/)
  blog post, with some action referencing a conduit (i.e., this is of the form
  `let conduit = .. in \w -> ..`), and then a top-level exception handler
  `retry` that might try an action again after it has executed.

* We set up test infrastructure so that we can verify _within the test suite_
  whether a particular case has the memory behaviour we expect. In addition,
  all test cases are marked with "OK" for tests that run in constant space
  versus "OOM" for tests that do not. We can therefore do a sanity check that
  the "OK" tests _really_ are OK by running

  ```
  cabal run test-dupIO -- -p OK +RTS -s
  ```



