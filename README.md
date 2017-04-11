# Collection-Strawman

[![Join the chat at https://gitter.im/scala/collection-strawman](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala/collection-strawman?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Prototype improvements for Scala collections.

- [Gitter Discussion](https://gitter.im/scala/collection-strawman)
- [Dotty Issue](https://github.com/lampepfl/dotty/issues/818)
- [Scala Center Proposal](https://github.com/scalacenter/advisoryboard/blob/master/proposals/007-collections.md)

## Use it in your project

We published a 0.1.0 version so that you can experiment with the new design.
Note that most of the collection implementations are incomplete!

~~~ scala
libraryDependencies += "ch.epfl.scala" %% "collection-strawman" % "0.1.0"
~~~

Only Scala 2.12 is supported so far.

We also automatically publish snapshots on Sonatype:

~~~ scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "ch.epfl.scala" %% "collection-strawman" % "0.2.0-SNAPSHOT"
~~~

## Implemented collection types

- [x] `List`
- [x] `LazyList` (aka `Stream` in 2.12)
- [ ] `Queue`
- [ ] `Stack`
- [x] `ArrayOps`
- [x] `StringOps`
- [x] `ArrayBuffer`
- [x] `ListBuffer`
- [ ] `UnrolledBuffer`
- [ ] `LinkedList`
- [ ] `DoubleLinkedList`
- [ ] `Range` / `NumericRange`
- [ ] `HashMap`
- [ ] `TreeMap`
- [ ] `IntMap` / `LongMap` (?)
- [ ] `ListMap`
- [ ] `MultiMap`
- [x] `HashSet`
- [x] `ListSet`
- [x] `TreeSet`
- [ ] `EqSet`
- [ ] `BitSet`
- [x] `View`

## Implemented operations (on the relevant collection types)

### Operations not returning a collection

- [x] `firstKey`
- [x] `forall`
- [x] `foreach`
- [x] `foldLeft`
- [x] `foldRight`
- [x] `head`
- [x] `indexWhere`
- [x] `isEmpty` / `nonEmpty`
- [x] `last`
- [x] `lastKey`
- [x] `mkString`
- [x] `size`
- [x] `sum`
- [x] `to`

### Transformations to collections having the same element type

- [x] `drop`
- [x] `empty`
- [x] `filter`
- [x] `partition`
- [x] `range`
- [x] `splitAt`
- [x] `tail`
- [x] `take`
- [ ] `groupBy`

### Transformations to collections that can have a different element type

- [x] `++` / `concat`
- [x] `flatMap`
- [x] `map`
- [x] `zip`

### In-place mutating operations

TODO