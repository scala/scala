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

## Contributing

See the [CONTRIBUTING](CONTRIBUTING.md) file.

## Implemented collection types

- [x] `List`
- [x] `LazyList` (aka `Stream` in 2.12)
- [ ] `Queue`
- [ ] `Stack`
- [x] `ArrayOps`
- [x] `StringOps`
- [x] `ArrayBuffer`
- [x] `ImmutableArray` (new)
- [x] `ListBuffer`
- [ ] `UnrolledBuffer`
- [ ] `LinkedList`
- [ ] `DoubleLinkedList`
- [x] `Range` / `NumericRange`
- [x] `Vector`
- [x] `HashMap`
- [x] `TreeMap`
- [ ] `IntMap` / `LongMap` (?)
- [x] `ListMap`
- [ ] `MultiMap`
- [x] `HashSet`
- [x] `ListSet`
- [x] `TreeSet`
- [ ] `EqSet`
- [x] `BitSet`
- [x] `View`

## Implemented operations (on the relevant collection types)

### Operations not returning a collection

- [x] `apply`
- [x] `contains`
- [x] `count`
- [x] `find`
- [x] `firstKey`
- [x] `forall` / `exists`
- [x] `foreach`
- [x] `foldLeft`
- [x] `foldRight`
- [x] `get`
- [x] `getOrElse`
- [x] `getOrElseUpdate`
- [x] `head`
- [x] `indexWhere`
- [x] `isDefinedAt`
- [x] `isEmpty` / `nonEmpty`
- [x] `keysIteratorFrom`
- [x] `last` / `lastOption`
- [x] `lastKey`
- [x] `max` / `maxBy`
- [x] `min` / `minBy`
- [x] `mkString`
- [x] `product`
- [x] `reduce` / `reduceOption` / `reduceLeft` / `reduceRight`
- [x] `size`
- [x] `span`
- [x] `sum`
- [x] `to`

### Transformations to collections having the same element type

- [x] `drop` / `dropRight` / `dropWhile`
- [x] `empty`
- [x] `filter` / `filterNot`
- [x] `groupBy`
- [x] `intersect`
- [x] `partition`
- [x] `range`
- [x] `rangeImpl`
- [x] `slice`
- [x] `splitAt`
- [x] `tail`
- [x] `init`
- [x] `take` / `takeRight` / `takeWhile`
- [x] `updated`

### Transformations to collections that can have a different element type

- [x] `updated`
- [x] `prepend`
- [x] `append`
- [x] `++` / `concat` / `union`
- [x] `flatMap`
- [x] `grouped`
- [x] `map`
- [x] `merged`
- [x] `scan` / `scanLeft` / `scanRight`
- [x] `sliding`
- [x] `unzip`
- [x] `zip` / `zipWithIndex`

### In-place mutating operations

TODO