# Todo Tasty

- [ ] Any tree possible could be in the parents of a class (e.g. blocks for default params out of order)
- [ ] arbitrary Annotations
- [ ] Refinement types
- [ ] scala 3 enum constants are not in the companion, but are a static field.
- [ ] java.lang.Enum empty ctor parent
- [ ] deeply recursive bounds on higher kinded types
- [ ] Need to detect better when a module is missing from classpath. (currently we get an error creating a singleton type from NoSymbol)
- [ ] Ignore open modifier
- [ ] check that the error in seeing `scala.collection.immutable.List` in `F[_] <: List[_]` is to do with module names
- [x] singleton types in a signature
- [x] more principled pattern exhaustivity analysis (Unpickling of annotations required)
- [x] scala.annotation.internal.Child Annotations

Dotty only

- [ ] Union types
- [ ] Function23+
- [ ] Tuple23+
- [ ] givenFunction
- [ ] scala 3 macros
