# Revision history for monad-effect

## 0.1.0.0 -- 2025-09-20

* First release of monad-effect

## 0.2.0.0 -- 2025-10-17

* Refactored `Control.System` to use bracket pattern by default, users of `Control.Monad.Effect` are not affected. Adding many new combinators.

## 0.2.1.0 -- 2025-11-05

* Exporting `lift`

## 0.2.2.0 -- 2025-12-02

* Adding `ResultT` synonym, `MonadExcept` class

* Adding `ReaderT` and `StateT` helpers

* Adding `withAsyncEffT'` helper, generalized certain type signatures

* Adding Exception instances and `tryAndThrow` IO utilities

## 0.2.3.0 -- 2025-12-13

* Fix bogus instance logic of NonEmptySubList, used in effThrowEListIn adding liftExceptT helpers

* Adding certain warnings and rename declareNoError to unsafeDeclareNoError

* Adding type-apply helpers like `applyMods`, `applyErrors`, etc.

* refine README.md
