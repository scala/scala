


import scala.math.Ordering



/** The heart of the problem - we want to retain the ordering when
 *  using `++` on sorted maps.
 *
 *  There are 2 `++` overloads - a generic one in traversables and
 *  a map-specific one in `MapLike` - which knows about the ordering.
 *
 *  The problem here is that the expected return type for the expression
 *  in which `++` appears drives the decision of the overload that needs
 *  to be taken.
 *  The `collection.SortedMap` does not have `++` overridden to return
 *  `SortedMap`, but `immutable.Map` instead.
 *  This is why `collection.SortedMap` used to resort to the generic
 *  `TraversableLike.++` which knows nothing about the ordering.
 *
 *  To avoid `collection.SortedMap`s resort to the more generic `TraversableLike.++`,
 *  we override the `MapLike.++` overload in `collection.SortedMap` to return
 *  the proper type `SortedMap`.
 */
object Test {

  def main(args: Array[String]) {
    testCollectionSorted()
    testImmutableSorted()
  }

  def testCollectionSorted() {
    import collection._
    val order = implicitly[Ordering[Int]].reverse
    var m1: SortedMap[Int, String] = SortedMap.empty[Int, String](order)
    var m2: SortedMap[Int, String] = SortedMap.empty[Int, String](order)

    m1 += (1 -> "World")
    m1 += (2 -> "Hello")

    m2 += (4 -> "Bar")
    m2 += (5 -> "Foo")

    val m3: SortedMap[Int, String] = m1 ++ m2

    println(m1)
    println(m2)
    println(m3)

    println(m1 + (3 -> "?"))
  }

  def testImmutableSorted() {
    import collection.immutable._
    val order = implicitly[Ordering[Int]].reverse
    var m1: SortedMap[Int, String] = SortedMap.empty[Int, String](order)
    var m2: SortedMap[Int, String] = SortedMap.empty[Int, String](order)

    m1 += (1 -> "World")
    m1 += (2 -> "Hello")

    m2 += (4 -> "Bar")
    m2 += (5 -> "Foo")

    val m3: SortedMap[Int, String] = m1 ++ m2

    println(m1)
    println(m2)
    println(m3)

    println(m1 + (3 -> "?"))
  }
}
