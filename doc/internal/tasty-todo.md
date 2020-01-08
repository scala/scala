# Todo Tasty

- [ ] fix pattern exhaustivity analysis
- [ ] classes where parents are in a Block
- [ ] java.lang.Enum empty ctor parent
- [ ] deeply recursive bounds on higher kinded types
- [ ] Need better error for when top level module is missing from classpath. (currently we get an error creating a singleton type from NoSymbol)
- [ ] Annotations
- [ ] Ignore open modifier
- [ ] check that the error in seeing `scala.collection.immutable.List` in `F[_] <: List[_]` is to do with module names
- [x] fix pattern reachability analysis
- [x] value class Extension methods
- [x] default parameters
- [x] overloads
- [x] Operator methods and backtick escaped names (generalize encoders that respect structure of names)

Dotty only

- [ ] Union types
- [ ] Function23+
- [ ] Tuple23+
- [ ] givenFunction
- [ ] scala 3 macros
