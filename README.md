# Collection-Strawman

[![Join the chat at https://gitter.im/scala/collection-strawman](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala/collection-strawman?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Waffle.io board](https://badge.waffle.io/scala/collection-strawman.svg?label=ready&title=Ready+issues)](http://waffle.io/scala/collection-strawman)

Prototype improvements for Scala collections.

- [Gitter Discussion](https://gitter.im/scala/collection-strawman)
- [Dotty Issue](https://github.com/lampepfl/dotty/issues/818)
- [Scala Center Proposal](https://github.com/scalacenter/advisoryboard/blob/master/proposals/007-collections.md)

## Current status

The strawman is available as a regular external library (see below usage
instructions). The collections live in the `strawman.collection` namespace
(instead of `scala.collection`).

Almost all operations and collection types of the current standard collections
are available. If you see something missing, please
[create an issue](https://github.com/scala/collection-strawman/issues/new).

## Use it in your project

### Build setup

Add the following dependency to your project:

~~~ scala
libraryDependencies += "ch.epfl.scala" %% "collection-strawman" % "0.8.0"
libraryDependencies += "ch.epfl.scala" %% "collections-contrib" % "0.8.0" // optional
~~~

The 0.8.0 version is compatible with Scala 2.13 and Dotty 0.6. Scala 2.12 is also supported
but you might encounter type inference issues with it.

We also automatically publish snapshots on Sonatype:

~~~ scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "ch.epfl.scala" %% "collection-strawman" % "0.9.0-SNAPSHOT"
~~~

The `collections-contrib` artifact provides additional operations on the collections (see the
[Additional operations](#additional-operations) section).

### API Documentation

- [`collection-strawman`](https://static.javadoc.io/ch.epfl.scala/collection-strawman_2.12/0.8.0/index.html)
- [`collections-contrib`](https://static.javadoc.io/ch.epfl.scala/collections-contrib_2.12/0.8.0/index.html)

### Migrating from the standard collections to the strawman

A tool is being developed to automatically migrate code that uses the standard
collection to use the strawman.

To use it, add the [scalafix](https://scalacenter.github.io/scalafix/) sbt plugin
to your build, as explained in
[its documentation](https://scalacenter.github.io/scalafix/#Installation).

Two situations are supported: (1) migrating a 2.12 code base to a 2.12 code base that
uses the collection strawman as a library (instead of the standard collections), and
(2) migrating a 2.12 code base to 2.13 code base.

The migration tool is not exhaustive and we will continue to improve
it over time. If you encounter a use case that’s not supported, please
report it as described in the
[contributing documentation](CONTRIBUTING.md#migration-tool).

#### Migrating a 2.12 code base to a 2.12 code base that uses the collection strawman as a library

Run the following sbt task on your project:

~~~
> scalafix https://github.com/scala/collection-strawman/raw/master/scalafix/2.12/rules/src/main/scala/fix/Collectionstrawman_v0.scala
~~~

In essence, the migration tool changes the imports in your source code
so that the strawman definitions are imported. It also rewrites
expressions that use an API that is different in the strawman.

#### Migrating a 2.12 code base to 2.13 code base

Run the following sbt task on your project:

~~~
> scalafix https://github.com/scala/collection-strawman/raw/master/scalafix/2.13/rules/src/main/scala/fix/Collectionstrawman_v0.scala
~~~

### Additional Operations

The `collections-contrib` artifact provides decorators enriching the collections with new operations. You can
think of this artifact as an incubator: if we get evidence that these operations should be part of the core,
we might eventually move them.

The new operations are provided via an implicit enrichment. You need to add the following import to make them
available:

~~~ scala
import strawman.collection.decorators._
~~~

The following operations are provided:

- `Seq`
    - `intersperse`
- `Map`
    - `zipByKey` / `join` / `zipByKeyWith`
    - `mergeByKey` / `fullOuterJoin` / `mergeByKeyWith` / `leftOuterJoin` / `rightOuterJoin`

The following collections are provided:

- `MultiSet` (both mutable and immutable)
- `SortedMultiSet` (both mutable and immutable)

## Roadmap

1. September 2017: release targeting Scala 2.13 and Dotty.
    - Implement most of the current collections types
    - Implement most of the current collections operations
    - Alternative to `CanBuildFrom` to get implicit builders
    - Include tests for correctness (taken from the current collections
      and from scala-collections-laws)
    - Provide a rewriting tool that migrates a code base from the current
      collections to the strawman
2. November 2017: move to the `scala` namespace
    - Create a branch of Scala 2.13 with the strawman instead of the current
      collections
3. January 2018: new features and performance improvements
    - Add Scala.js support
    - Consider the inclusion of new collection types (such as `Spandex`, `Steque`
      or `ArrayDeque`)
    - Consider the introduction of new operations (such as database-like joins,
      variants of groupBy, etc.)
    - Java interoperability
    - Separate project for parallel collections

## Contributing

We welcome contributions!

For more information, see the [CONTRIBUTING](CONTRIBUTING.md) file.

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
- [x] `mutable.LinkedHashMap`
- [x] `TreeMap`
- [ ] `IntMap` / `LongMap` (?)
- [x] `ListMap`
- [ ] `MultiMap`
- [x] `HashSet`
- [x] `mutable.LinkedHashSet`
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
- [x] `iteratorFrom` / `keysIteratorFrom` / `valuesIteratorFrom`
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
- [x] `distinct` / `distinctBy`
- [x] `drop` / `dropRight` / `dropWhile`
- [x] `empty`
- [x] `filter` / `filterNot` / `filterKeys`
- [x] `groupBy`
- [x] `init`
- [x] `intersect`
- [x] `partition`
- [x] `range` / `rangeTo`
- [x] `rangeImpl`
- [x] `sorted` / `sortBy` / `sortWith`
- [x] `slice`
- [x] `splitAt`
- [x] `substetOf`
- [x] `subsets`
- [x] `tail`
- [x] `take` / `takeRight` / `takeWhile`
- [x] `updated`
- [x] `withDefault` / `withDefaultValue`

### Transformations to collections that can have a different element type

- [x] `combinations`
- [x] `updated`
- [x] `prepend`
- [x] `append`
- [x] `++` / `concat` / `union`
- [x] `++:` / `prependAll`
- [x] `flatMap`
- [x] `grouped`
- [x] `keys` / `keySet` / `keysIterator`
- [x] `map` / `mapValues`
- [x] `merged`
- [x] `padTo`
- [x] `permutations`
- [x] `scan` / `scanLeft` / `scanRight`
- [x] `sliding`
- [x] `unzip`
- [x] `values` / `valuesIterator`
- [x] `zip` / `zipWithIndex` / `lazyZip`

### In-place mutating operations

TODO