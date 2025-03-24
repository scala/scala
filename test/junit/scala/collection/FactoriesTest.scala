package scala.collection

import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue}
import org.junit.Test

import scala.collection.mutable.ArrayBuffer

import scala.collection.{immutable => im}

@annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
class FactoriesTest {

  val seq: Seq[Int] = ArrayBuffer(1, 2, 3)

  @Test def buildFromUsesSourceCollectionFactory(): Unit = {

    def cloneCollection[A, C](xs: Iterable[A])(implicit bf: BuildFrom[xs.type, A, C]): C =
      bf.fromSpecific(xs)(xs)

    assertEquals("ArrayBuffer", cloneCollection(seq).collectionClassName)
  }

  @Test def factoryIgnoresSourceCollectionFactory(): Unit = {

    def cloneElements[A, C](xs: Iterable[A])(cb: Factory[A, C]): C =
      cb.fromSpecific(xs)

    assertEquals("List", cloneElements(seq)(Seq).collectionClassName)
  }

  def apply(factory: IterableFactory[Iterable]): Unit = {
    assertTrue(factory(1, 2, 3).iterator.sameElements(new View.Elems(1, 2, 3)))
  }

  def iterate(factory: IterableFactory[Iterable]): Unit = {
    val iterable = factory.iterate(0, 10)(x => x + 1)
    val expectedValues = immutable.Range(0, 10)
    assertEquals(expectedValues.size, iterable.size)
    assertTrue(iterable.forall(expectedValues.contains))
  }

  def unfold(factory: IterableFactory[Iterable]): Unit = {
    val iterable = factory.unfold(0)(i => if (i >= 10) None else Some((i, i + 1)))
    val expectedValues = immutable.Range(0, 10)
    assertEquals(expectedValues.size, iterable.size)
    assertTrue(iterable.forall(expectedValues.contains))
  }

  def range(factory: IterableFactory[Iterable]): Unit = {
    val iterable = factory.range(0, 10)
    val expectedValues = immutable.Range(0, 10)
    assertEquals(expectedValues.size, iterable.size)
    assertTrue(iterable.forall(expectedValues.contains))
  }

  def fill(factory: SeqFactory[Seq]): Unit = {
    val seq = factory.fill(10)(42)
    assertEquals(10, seq.size)
    assertTrue(seq.forall(_ == 42))

    val seq2 = factory.fill(10, 20)(42)
    assertEquals(10, seq2.size)
    seq2.foreach { seq3 =>
      assertEquals(20, seq3.size)
      assertTrue(seq3.forall(_ == 42))
    }
  }

  def tabulate(factory: SeqFactory[Seq]): Unit = {
    val seq = factory.tabulate(10)(x => x * x)
    assertEquals(10, seq.size)
    assertTrue(seq.zipWithIndex.forall { case (x, i) => x == i * i })

    val seq2 = factory.tabulate(10, 20)((i, j) => i * j)
    assertEquals(10, seq2.size)
    seq2.zipWithIndex.foreach { case (seq3, i) =>
      assertEquals(20, seq3.size)
      assertTrue(seq3.zipWithIndex.forall { case (x, j) => x == i * j })
    }
  }


  @Test
  def testFactories(): Unit = {
    val seqFactories: List[SeqFactory[Seq]] =
      List[SeqFactory[Seq]](
        immutable.List,
        immutable.LazyList,
        immutable.Vector,
        immutable.ArraySeq.untagged,
        mutable.ListBuffer,
        mutable.ArrayBuffer
      )

    val iterableFactories: List[IterableFactory[Iterable]] =
      immutable.HashSet ::
        mutable.HashSet ::
        (seqFactories: List[IterableFactory[Iterable]])

    iterableFactories.foreach(apply)
    iterableFactories.foreach(iterate)
    iterableFactories.foreach(unfold)
    iterableFactories.foreach(range)

    seqFactories.foreach(fill)
    seqFactories.foreach(tabulate)
  }

  def factoryFromIterableOnceReturnsSameReference[CC[X] <: IterableOnce[X] , A](factories: IterableFactory[CC]*)(inputs: CC[_]*): Unit =
    for {
      factory <- factories
      input <- inputs
    } {
      assertSame(
        s"IterableFactory ($factory)'s method `from` should return the same reference when passed $input",
        input,
        factory.from(input))
    }
  def sortedFactoryFromIterableOnceReturnsSameReference[CC[X] <: IterableOnce[X] , A](
    factories: SortedIterableFactory[CC]*
    )(
      inputs: CC[A]*
    )(implicit ordering: Ordering[A]): Unit =
    for {
      factory <- factories
      input <- inputs
    } {
      assertSame(
        s"SortedIterableFactory ($factory)'s method `from` should return the same reference when passed $input",
        input,
        factory.from(input))
    }
  def mapFactoryFromIterableOnceReturnsSameReference[CC[X, Y] <: Map[X, Y] , K, V](
   factories: MapFactory[CC]*
    )(
     inputs: CC[K, V]*
   ): Unit =
    for {
      factory <- factories
      input <- inputs
    } {
      assertSame(
        s"MapFactory ($factory)'s method `from` should return the same reference when passed $input",
        input,
        factory.from(input))
    }
  def sortedMapFactoryFromIterableOnceReturnsSameReference[CC[X, Y] <: Map[X, Y] , K, V](
    factories: SortedMapFactory[CC]*
  )(
    inputs: CC[K, V]*
  )(implicit ordering: Ordering[K]): Unit =
    for {
      factory <- factories
      input <- inputs
    } {
      assertSame(
        s"SortedMapFactory ($factory)'s method `from` should return the same reference when passed $input",
        input,
        factory.from(input))
    }


  @deprecated("Uses deprecated Stream", since="2.13.0")
  @Test
  def testFactoriesReuseCollectionsWhenPossible(): Unit = {

    factoryFromIterableOnceReturnsSameReference(Iterable, im.Iterable)(
      SortedSet(1, 2, 3),
      im.SortedSet(1, 2, 3),
      im.TreeSet(1, 2, 3),
      im.TreeMap(1 -> 1, 2 -> 1, 3 -> 1).keySet,
      new Enumeration {
        val x, y, z = Value
      }.values,
      im.BitSet(1, 2, 3),
      im.Map(1 -> 2),
      im.HashMap(1 -> 2),
      im.IntMap(1 -> 2),
      im.LongMap(1L -> 2)
    )

    factoryFromIterableOnceReturnsSameReference(Iterator)(
      Iterator(1),
      List(1, 2, 3).iterator
    )

    factoryFromIterableOnceReturnsSameReference(LinearSeq, im.LinearSeq, Seq, im.Seq, Iterable, im.Iterable)(
      im.LinearSeq(),
      List(1, 2, 3),
      Stream(1, 2, 3),
      LazyList(1, 2, 3),
      im.Queue(1, 2, 3)
    )

    factoryFromIterableOnceReturnsSameReference(IndexedSeq, im.IndexedSeq, Seq, im.Seq, Iterable, im.Iterable)(
      Vector(),
      Vector(1, 2, 3),
      im.ArraySeq(1, 2, 3),
      1 to 5,
      "hello"
    )

    factoryFromIterableOnceReturnsSameReference(Set, im.Set, Iterable, im.Iterable)(
      im.Set(1),
      im.HashSet("a", "b", "c"),
      im.ListSet('c', 'd'),
      im.Map("a" -> 1, "b" -> 1, "c" -> 1).keySet,      // MapOps$ImmutableKeySet
      im.HashMap("a" -> 1, "b" -> 1, "c" -> 1).keySet,  // HashKeySet
    )

    sortedFactoryFromIterableOnceReturnsSameReference(SortedSet, im.SortedSet)(
      SortedSet(1, 2, 3),
      im.SortedSet(1, 2, 3),
      im.TreeSet(1, 2, 3),
      im.TreeMap(1 -> 1, 2 -> 1, 3 -> 1).keySet,
      im.BitSet(1, 2, 3)
    )

    object `enum` extends Enumeration {
      val x, y, z = Value
    }

    val enumValues = `enum`.values

    sortedFactoryFromIterableOnceReturnsSameReference(SortedSet, im.SortedSet)(enumValues)

    mapFactoryFromIterableOnceReturnsSameReference(Map, im.Map)(im.Map(1 -> 2), im.HashMap(1 -> 2))
    mapFactoryFromIterableOnceReturnsSameReference(im.HashMap)(im.HashMap(1 -> 2))
    // unsound due to widening, scala/bug#12745
    //mapFactoryFromIterableOnceReturnsSameReference(Map, im.Map)(im.IntMap(1 -> 2))
    //mapFactoryFromIterableOnceReturnsSameReference(Map, im.Map)(im.LongMap(1L -> 2))

    mapFactoryFromIterableOnceReturnsSameReference(im.SeqMap, Map, im.Map)(
      im.ListMap(1 -> 2),
      im.VectorMap(1 -> 2),
      im.TreeSeqMap(1 -> 2, 3 -> 4)
    )

    def customSortedMap(implicit ord: Ordering[Int]): SortedMap[Int, Int] =
      new im.SortedMap[Int, Int] {
        override def updated[V1 >: Int](key: Int, value: V1): im.SortedMap[Int, V1] = null
        override def removed(key: Int): im.SortedMap[Int, Int] = null
        override def iteratorFrom(start: Int): Iterator[(Int, Int)] = null
        override def keysIteratorFrom(start: Int): Iterator[Int] = null
        override implicit def ordering: Ordering[Int] = ord
        override def rangeImpl(from: Option[Int], until: Option[Int]): im.SortedMap[Int, Int] = null
        override def get(key: Int): Option[Int] = null
        override def iterator: Iterator[(Int, Int)] = null
        override def toString(): String = "BogusSortedMap"
      }

    sortedMapFactoryFromIterableOnceReturnsSameReference(SortedMap, im.SortedMap)(
      im.TreeMap(1 -> 1),
      im.TreeMap(1 -> 1).withDefaultValue(1),
      customSortedMap
    )
  }


  @Test
  def delegatingSeqFactoriesDelegateCreationFromVarArgs(): Unit = {
    assert(im.IndexedSeq().isInstanceOf[Vector[_]], "immutable.IndexedSeq.apply should delegate to Vector.apply")
    assert(im.IndexedSeq(1,2,3).isInstanceOf[Vector[_]], "immutable.IndexedSeq.apply should delegate to Vector.apply")
    assert(IndexedSeq().isInstanceOf[Vector[_]], "IndexedSeq.apply should delegate to Vector.apply")
    assert(IndexedSeq(1,2,3).isInstanceOf[Vector[_]], "IndexedSeq.apply should delegate to Vector.apply")

    assert(im.Seq().isInstanceOf[List[_]], "immutable.Seq.apply should delegate to List.apply")
    assert(im.Seq(1,2,3).isInstanceOf[List[_]], "immutable.Seq.apply should delegate to List.apply")
    assert(Seq().isInstanceOf[List[_]], "Seq.apply should delegate to List.apply")
    assert(Seq(1,2,3).isInstanceOf[List[_]], "Seq.apply should delegate to List.apply")

    assert(im.LinearSeq().isInstanceOf[List[_]], "immutable.LinearSeq.apply should delegate to List.apply")
    assert(im.LinearSeq(1,2,3).isInstanceOf[List[_]], "immutable.LinearSeq.apply should delegate to List.apply")
    assert(LinearSeq().isInstanceOf[List[_]], "LinearSeq.apply should delegate to List.apply")
    assert(LinearSeq(1,2,3).isInstanceOf[List[_]], "LinearSeq.apply should delegate to List.apply")

    assert(im.Iterable().isInstanceOf[List[_]], "immutable.Iterable.apply should delegate to List.apply")
    assert(im.Iterable(1,2,3).isInstanceOf[List[_]], "immutable.Iterable.apply should delegate to List.apply")
    assert(Iterable().isInstanceOf[List[_]], "Iterable.apply should delegate to List.apply")
    assert(Iterable(1,2,3).isInstanceOf[List[_]], "Iterable.apply should delegate to List.apply")

    assert(im.SeqMap().getClass.getSimpleName == "EmptySeqMap$", "immutable.SeqMap.apply should use EmptySeqMap$")
    assert(im.SeqMap(1 -> 2, 3 -> 4, 5 -> 6).getClass.getSimpleName == "SeqMap3", "immutable.SeqMap.apply should use SeqMap3")
    assert(im.SeqMap(1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8, 9 -> 10).isInstanceOf[im.VectorMap[_, _]], "immutable.SeqMap.apply should delegate to VectorMap.apply")

    assert(Map().isInstanceOf[im.Map[_, _]], "Map.apply should delegate to immutable.Map.apply")
    assert(Map(1 -> 2, 3 -> 4, 5 -> 6).isInstanceOf[im.Map[_, _]], "Map.apply should delegate to immutable.Map.apply")

    assert(SortedMap[Int, Int]().isInstanceOf[im.TreeMap[_, _]], "SortedMapMap.apply should delegate to immutable.TreeMap.apply")
    assert(im.SortedMap[Int, Int]().isInstanceOf[im.TreeMap[_, _]], "immutable.SortedMapMap.apply should delegate to immutable.TreeMap.apply")
    assert(SortedMap(1 -> 2, 3 -> 4, 5 -> 6).isInstanceOf[im.TreeMap[_, _]], "SortedMap.apply should delegate to immutable.TreeMap.apply")
    assert(im.SortedMap(1 -> 2, 3 -> 4, 5 -> 6).isInstanceOf[im.TreeMap[_, _]], "immutable.SortedMap.apply should delegate to immutable.TreeMap.apply")

  }

  @Test def `custom set requires rebuild`: Unit = {
    import scala.collection.immutable.{Set, SortedSet}
    def testSame(xs: Set[Int], ys: Set[Any]): Boolean = {
      assertFalse(ys("oops"))
      assertFalse(ys.contains("oops"))
      xs.eq(ys)
    }
    val s1 = Set(42)
    assertTrue(testSame(s1, Set.from(s1)))
    val ss = SortedSet(42)
    assertFalse(testSame(ss, Set.from(ss)))

    class Custom extends Set[Int] {
      // Members declared in scala.collection.IterableOnce
      def iterator: Iterator[Int] = Iterator.empty // implements `def iterator: Iterator[A]`

      // Members declared in scala.collection.SetOps
      def contains(elem: Int): Boolean = ??? // implements `def contains(elem: A): Boolean`

      // Members declared in scala.collection.immutable.SetOps
      def excl(elem: Int): scala.collection.immutable.Set[Int] = ??? // implements `def excl(elem: A): C`
      def incl(elem: Int): scala.collection.immutable.Set[Int] = ??? // implements `def incl(elem: A): C`
    }
    val custom = new Custom
    assertFalse(testSame(custom, Set.from(custom)))
  }

  @Test def `select maps do not require rebuild`: Unit = {
    import scala.collection.immutable.{IntMap, ListMap, SeqMap, SortedMap, TreeSeqMap, VectorMap}

    object X {
      val iter: Iterable[(Any, String)] = List(1, 2, 3, 4, 5).map(i => i -> i.toString).to(SortedMap.sortedMapFactory)
      val set: Map[Any, String] = Map.from(iter)
    }

    // where ys is constructed from(xs), verify no CEE, return true if same
    def testSame(xs: Map[Int, String], ys: Map[Any, String]): Boolean = {
      assertTrue(ys.get("oops").isEmpty)
      assertFalse(ys.contains("oops"))
      xs.eq(ys)
    }
    assertFalse(X.set.contains("oops")) // was CCE
    // exercise small Maps
    1.to(5).foreach { n =>
      val m = Map(1.to(n).map(i => i -> i.toString): _*)
      assertTrue(testSame(m, Map.from(m)))
    }
    // other Maps that don't require rebuilding
    val listMap = ListMap(42 -> "four two")
    assertTrue(testSame(listMap, Map.from(listMap)))
    val treeSeqMap = TreeSeqMap(42 -> "four two")
    assertTrue(testSame(treeSeqMap, Map.from(treeSeqMap)))
    val vectorMap = VectorMap(42 -> "four two")
    assertTrue(testSame(vectorMap, Map.from(vectorMap)))
    val seqMap = SeqMap.empty[Int, String] + (42 -> "four two")
    assertTrue(testSame(seqMap, Map.from(seqMap)))
    // require rebuilding
    val sordid = SortedMap(42 -> "four two")
    assertFalse(testSame(sordid, Map.from(sordid)))
    val defaulted = listMap.withDefault(_.toString * 2)
    assertFalse(testSame(defaulted, Map.from(defaulted)))   // deoptimized, see desorted
    val desorted = sordid.withDefault(_.toString * 2)
    assertFalse(testSame(desorted, Map.from(desorted)))

    assertTrue(Map.from(IntMap(42 -> "once", 27 -> "upon"): Iterable[(Any, String)]).get("a time").isEmpty)
  }

// java.lang.ClassCastException: class java.lang.String cannot be cast to class java.lang.Integer

  implicitly[Factory[Char, String]]
  implicitly[Factory[Char, Array[Char]]]
  implicitly[Factory[Int, BitSet]]
  implicitly[Factory[Int, mutable.BitSet]]
  implicitly[Factory[Int, immutable.BitSet]]
  implicitly[Factory[(Int, Boolean), immutable.IntMap[Boolean]]]
  implicitly[Factory[(Long, Boolean), immutable.LongMap[Boolean]]]
  implicitly[Factory[(Long, Boolean), mutable.LongMap[Boolean]]]
  implicitly[Factory[(String, Boolean), mutable.AnyRefMap[String, Boolean]]]
}
