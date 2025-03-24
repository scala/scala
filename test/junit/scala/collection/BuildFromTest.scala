package scala.collection

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue}

import scala.math.Ordering

import mutable.{ArrayBuffer, Builder}

@annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
class BuildFromTest {

  // You can either overload methods for IterableOps and Iterable with SortedOps (if you want to support constrained collection types)
  def optionSequence1[CC[X] <: IterableOps[X, CC, _], A](xs: CC[Option[A]]): Option[CC[A]] =
    xs.foldLeft[Option[Builder[A, CC[A]]]](Some(xs.iterableFactory.newBuilder[A])) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result())
  def optionSequence1[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, CC[X]], A : Ordering](xs: CC[Option[A]]): Option[CC[A]] =
    xs.foldLeft[Option[Builder[A, CC[A]]]](Some((xs: SortedSetOps[Option[A], CC, CC[Option[A]]] /*TODO why is this ascription needed? introduced with #7929*/).sortedIterableFactory.newBuilder[A])) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result())

  // ...or use BuildFrom to abstract over both and also allow building arbitrary collection types
  def optionSequence2[CC[X] <: Iterable[X], A, To](xs: CC[Option[A]])(implicit bf: BuildFrom[CC[Option[A]], A, To]): Option[To] =
    xs.foldLeft[Option[Builder[A, To]]](Some(bf.newBuilder(xs))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result())

  // Using dependent types:
  def optionSequence3[A, To](xs: Iterable[Option[A]])(implicit bf: BuildFrom[xs.type, A, To]): Option[To] =
    xs.foldLeft[Option[Builder[A, To]]](Some(bf.newBuilder(xs))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result())

  def eitherSequence[A, B, To](xs: Iterable[Either[A, B]])(implicit bf: BuildFrom[xs.type, B, To]): Either[A, To] =
    xs.foldLeft[Either[A, Builder[B, To]]](Right(bf.newBuilder(xs))) {
      case (Right(builder), Right(b)) => Right(builder += b)
      case (Left(a)       ,        _) => Left(a)
      case (_             ,  Left(a)) => Left(a)
    }.map(_.result())

  @Test
  def optionSequence1Test(): Unit = {
    val xs1 = List(Some(1), None, Some(2))
    val o1 = optionSequence1(xs1)
    val o1t: Option[List[Int]] = o1
    assertTrue(o1t.isEmpty)

    val xs2: immutable.TreeSet[Option[String]] = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence1(xs2)
    val o2t: Option[immutable.Set[String]] = o2
    assertTrue(o2t.isEmpty)

    val xs4 = List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence1(xs4)
    val o4t: Option[List[(Int, String)]] = o4
    assertEquals(Some(List(1 -> "a", 2 -> "b")), o4t)
    val o5: Option[immutable.TreeMap[Int, String]] = o4.map(_.to(immutable.TreeMap))
    assertEquals(Some(immutable.TreeMap(1 -> "a", 2 -> "b")), o5)
  }

  @Test
  def optionSequence2Test(): Unit = {
    val xs1 = List(Some(1), None, Some(2))
    val o1 = optionSequence2(xs1)
    val o1t: Option[immutable.List[Int]] = o1
    assertTrue(o1t.isEmpty)

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence2(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2
    assertTrue(o2t.isEmpty)

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence2(xs4)(immutable.TreeMap) // same syntax as in `.to`
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
    assertEquals(Some(immutable.TreeMap(1 -> "a", 2 -> "b")), o4t)
  }

  @Test
  def optionSequence3Test(): Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence3(xs1)
    val o1t: Option[immutable.List[Int]] = o1
    assertTrue(o1t.isEmpty)

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence3(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2
    assertTrue(o2t.isEmpty)

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence3(xs4)(immutable.TreeMap) // same syntax as in `.to`
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
    assertEquals(Some(immutable.TreeMap(1 -> "a", 2 -> "b")), o4t)
  }

  @Test
  def eitherSequenceTest(): Unit = {
    val xs3 = mutable.ListBuffer(Right("foo"), Left(0), Right("bar"))
    val e1 = eitherSequence(xs3)
    val e1t: Either[Int, mutable.ListBuffer[String]] = e1
    assertTrue(e1t.isLeft)
  }

  // From https://github.com/scala/collection-strawman/issues/44
  def flatCollect[A, B, To](coll: Iterable[A])(f: PartialFunction[A, IterableOnce[B]])
                       (implicit bf: BuildFrom[coll.type, B, To]): To = {
    val builder = bf.newBuilder(coll)
    for (a <- coll) {
      if (f.isDefinedAt(a)) builder ++= f(a)
    }
    builder.result()
  }

  def partitionMap[A, B, C, ToL, ToR](coll: Iterable[A])(f: A => Either[B, C])
              (implicit bfLeft:  BuildFrom[coll.type, B, ToL], bfRight: BuildFrom[coll.type, C, ToR]): (ToL, ToR) = {
    val left = bfLeft.newBuilder(coll)
    val right = bfRight.newBuilder(coll)
    for (a <- coll)
      f(a).fold(left.addOne, right.addOne)
    (left.result(), right.result())
  }


  @Test
  def flatCollectTest(): Unit = {
    val xs1 = List(1, 2, 3)
    val xs2 = flatCollect(xs1) { case 2 => ArrayBuffer("foo", "bar") }
    val xs3: List[String] = xs2
    assertEquals(List("foo", "bar"), xs3)

    val xs4 = immutable.TreeMap((1, "1"), (2, "2"))
    val xs5 = flatCollect(xs4) { case (2, v) => immutable.List((v, v)) }
    val xs6: immutable.TreeMap[String, String] = xs5
    assertEquals(immutable.TreeMap("2" -> "2"), xs6)

    val xs7 = immutable.HashMap((1, "1"), (2, "2"))
    val xs8 = flatCollect(xs7) { case (2, v) => immutable.List((v, v)) }
    val xs9: immutable.HashMap[String, String] = xs8
    assertEquals(immutable.HashMap("2" -> "2"), xs9)

    val xs10 = immutable.TreeSet(1, 2, 3)
    val xs11 = flatCollect(xs10) { case 2 => immutable.List("foo", "bar") }
    val xs12: immutable.TreeSet[String] = xs11
    assertEquals(immutable.TreeSet("foo", "bar"), xs12)
  }

  @Test
  def partitionMapTest(): Unit = {
    val xs1 = List(1, 2, 3)
    val (xs2, xs3) = partitionMap(xs1)(x => if (x % 2 == 0) Left(x) else Right(x.toString))
    val xs4: List[Int] = xs2
    assertEquals(List(2), xs4)
    val xs5: List[String] = xs3
    assertEquals(List("1", "3"), xs5)

    val xs6 = immutable.TreeMap((1, "1"), (2, "2"))
    val (xs7, xs8) = partitionMap(xs6) { case (k, v) => Left[(String, Int), (Int, Boolean)]((v, k)) }
    val xs9: immutable.TreeMap[String, Int] = xs7
    assertEquals(immutable.TreeMap(("1", 1), ("2", 2)), xs9)
    val xs10: immutable.TreeMap[Int, Boolean] = xs8
    assertTrue(xs10.isEmpty)
  }

  @Test
  def buildFromToFactory(): Unit = {
    val bf = implicitly[BuildFrom[Iterable[Int], Int, Iterable[Int]]]
    val f = bf.toFactory(Set.empty[Int])
    val bs = f.fromSpecific(Iterator(1, 2, 3))
    bs.asInstanceOf[Set[Int]]: Unit
  }

  implicitly[BuildFrom[String, Char, String]]
  implicitly[BuildFrom[Array[Int], Char, Array[Char]]]
  implicitly[BuildFrom[BitSet, Int, BitSet]]
  implicitly[BuildFrom[immutable.BitSet, Int, immutable.BitSet]]
  implicitly[BuildFrom[mutable.BitSet, Int, mutable.BitSet]]
  implicitly[BuildFrom[immutable.IntMap[_], (Int, String), immutable.IntMap[String]]]
  implicitly[BuildFrom[mutable.LongMap[_], (Long, String), mutable.LongMap[String]]]
  implicitly[BuildFrom[immutable.LongMap[_], (Long, String), immutable.LongMap[String]]]
  implicitly[BuildFrom[mutable.AnyRefMap[_ <: AnyRef, _], (String, String), mutable.AnyRefMap[String, String]]]
  implicitly[BuildFrom[mutable.AnyRefMap[String, String], (String, String), _]]

  // Check that collection companions can implicitly be converted to a `BuildFrom` instance
  Iterable: BuildFrom[_, Int, Iterable[Int]]
  Map: BuildFrom[_, (Int, String), Map[Int, String]]
  SortedSet: BuildFrom[_, Int, SortedSet[Int]]
  SortedMap: BuildFrom[_, (Int, String), SortedMap[Int, String]]
  immutable.IntMap: BuildFrom[_, (Int, String), immutable.IntMap[String]]
  immutable.LongMap: BuildFrom[_, (Long, String), immutable.LongMap[String]]
  mutable.LongMap: BuildFrom[_, (Long, String), mutable.LongMap[String]]
  mutable.AnyRefMap: BuildFrom[_, (String, String), mutable.AnyRefMap[String, String]]

  // Check that we don't get an implicit divergence in a futile part of the search tree:
  {
    sealed trait GPoint
    sealed trait HNil extends GPoint
    class HCons[H, +T <: GPoint] extends GPoint
    abstract class ExtendsOrdered extends Ordered[ExtendsOrdered]


    // In scala 2.13, this implicit search considers BuildFrom.buildFromSortedSetOps
    // which looks for a dep. implicit of type Ordering[(Int, HCons[ExtendsOrdered, HNil])]
    implicitly[collection.BuildFrom[Seq[Any], (Int, HCons[ExtendsOrdered, HNil]), Seq[(Int, HCons[ExtendsOrdered, HNil])]]]

    //
    // In Scala 2.12, buildFromSortedSetOps is not a candidate because it is in the companion object of
    // the SortedSet hierarchy, which is not part of the implicit scope for this search.
    // In 2.13, the implicit was moved to `object BuildFrom`, so _is_ considered
    //
    // The dependent implicit search:
    //
    // implicitly[(Int, HCons[ExtendsOrdered, HNil])]
    //
    // ... diverges on both Scala 2.12 and 2.13
    //
    // error: diverging implicit expansion for type scala.math.Ordering.AsComparable[(Int, HCons[ExtendsOrdered,HNil])]
    // starting with method orderingToOrdered in object Ordered
    //
    // Divergences in Ordering implicits are a long standing problem, but I always thought it too hard to
    // fix while retaining source compatibility.
    //
    // Removing `extends Ordered[X]` avoids divergence, but I'm not sure why. I diffed the -Vtyper log but
    // can't figure out why that is relevant.
    //
    // (In the original code, ExtendsOrdered was actually scala.Enumeration.Value, which does extends Ordered.
    //
    //
  }
  // additional test for previous, via scala/bug#12104
  locally {
    import scala.concurrent._, ExecutionContext.Implicits._
    Future.traverse(List(1))(_ => Future.failed(new NoSuchElementException))
    // diverging implicit expansion for type scala.collection.BuildFrom[List[Int],B,List[B] starting with method Tuple9 in object Ordering
  }
}
