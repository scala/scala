# Collection-Strawman

[![Join the chat at https://gitter.im/scala/collection-strawman](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala/collection-strawman?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Prototype improvements for Scala collections.

- [Gitter Discussion](https://gitter.im/scala/collection-strawman)
- [Dotty Issue](https://github.com/lampepfl/dotty/issues/818)
- [Scala Center Proposal](https://github.com/scalacenter/advisoryboard/blob/master/proposals/007-collections.md)

## Use it in your project

Add the following dependency to your project:

~~~ scala
libraryDependencies += "ch.epfl.scala" %% "collection-strawman" % "0.2.0"
~~~

The 0.2.0 version is compatible with Scala 2.13 and Dotty. Scala 2.12 is also supported
but you might encounter type inference issues with it.

We also automatically publish snapshots on Sonatype:

~~~ scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "ch.epfl.scala" %% "collection-strawman" % "0.3.0-SNAPSHOT"
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
- [x] `containsSlice`
- [x] `count`
- [x] `endsWith`
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
- [x] `indexOf` / `indexWhere` / `lastIndexOf` / `lastIndexWhere` / `indexOfSlice` / `lastIndexOfSlice`
- [x] `indices`
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
- [x] `startsWith`
- [x] `sum`
- [x] `to`

### Transformations to collections having the same element type

- [x] `diff`
- [x] `drop` / `dropRight` / `dropWhile`
- [x] `empty`
- [x] `filter` / `filterNot`
- [x] `groupBy`
- [x] `init`
- [x] `intersect`
- [x] `partition`
- [x] `range`
- [x] `rangeImpl`
- [x] `sorted` / `sortBy` / `sortWith`
- [x] `slice`
- [x] `splitAt`
- [x] `substetOf`
- [x] `subsets`
- [x] `tail`
- [x] `take` / `takeRight` / `takeWhile`
- [x] `updated`

### Transformations to collections that can have a different element type

- [x] `combinations`
- [x] `updated`
- [x] `prepend`
- [x] `append`
- [x] `++` / `concat` / `union`
- [x] `flatMap`
- [x] `grouped`
- [x] `map`
- [x] `merged`
- [x] `padTo`
- [x] `permutations`
- [x] `scan` / `scanLeft` / `scanRight`
- [x] `sliding`
- [x] `unzip`
- [x] `zip` / `zipWithIndex`

### In-place mutating operations

TODO