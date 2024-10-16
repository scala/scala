package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._

import scala.collection.{IterableOnce, Iterator, SeqFactory}

class LazyListLazinessTest {
  import LazyListLazinessTest._

  /* op laziness tests */

  // if this fails, all the rest will fail
  @Test
  def opLazinessChecker_correctness(): Unit = {
    val checker = new OpLazinessChecker
    val illegalState = (s: String) => new IllegalStateException("correctness check failed: " + s)

    // check that none start evaluated
    checker.assertAll(evaluated = false, illegalState)

    // check that it detects state evaluation
    checker.lazyList.isEmpty
    checker.assert(evaluated = true, 0)
    checker.assert(evaluated = false, 1)
    checker.lazyList.tail.isEmpty
    checker.assert(evaluated = true, 1)
    checker.assert(evaluated = false, 2)

    // check that all are evaluated after forcing
    checker.lazyList.force

    checker.assertAll(evaluated = true, illegalState)

    // check unmodified checker is properly lazy
    val op = lazyListOp(x => x)
    assertRepeatedlyLazy(op)
    assertLazyNextStateWhenStateEvaluated(op) // checked for completeness
    assertLazyAll(op)
    assertLazyAllSkipping(op, 0) // checked for completeness
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  private def genericLazyOp_properlyLazy(op: LazyListToLazyListOp, d: DropProfile = NoDrops): Unit = {
    assertLazyAll(op)
    assertRepeatedlyLazy(op, d)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def head_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.head, 1)
  }

  @Test
  def tail_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.tail, 1)
  }

  @Test
  def knownSize_properlyLazy(): Unit = {
    assertLazyAll(_.knownSize)
  }

  @Test
  def isEmpty_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.isEmpty, 1)
  }

  @Test
  def nonEmpty_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.nonEmpty, 1)
  }

  private def genericFilter_properlyLazy(filter: (LazyList[Int], Int => Boolean) => LazyList[Int],
                                         isFlipped: Boolean): Unit = {
    genericLazyOp_properlyLazy(filter(_, _ => !isFlipped))
    genericLazyOp_properlyLazy(filter(_, i => (i % 2 != 0) == isFlipped), DropProfile(dropCount = 1, repeatedDrops = true))
  }

  @Test
  def filter_properlyLazy(): Unit = {
    genericFilter_properlyLazy(_ filter _, isFlipped = false)
  }

  @Test
  def filterNot_properlyLazy(): Unit = {
    genericFilter_properlyLazy(_ filterNot _, isFlipped = true)
  }

  @Test
  def withFilter_properlyLazy(): Unit = {
    assertLazyAll(_.withFilter(_ => true))
    assertLazyAll(_.withFilter(_ % 2 != 0))

    genericFilter_properlyLazy(_ withFilter _ map identity, isFlipped = false)
  }

  private def genericPartition_properlyLazy(partition: LazyList[Int] => (LazyList[Int], LazyList[Int])): Unit = {
    val op1 = partition.andThen(_._1)
    val op2 = partition.andThen(_._2)
    val d = DropProfile(dropCount = 1, repeatedDrops = true)
    for (op <- op1 :: op2 :: Nil) genericLazyOp_properlyLazy(op, d)
  }

  @Test
  def partition_properlyLazy(): Unit = {
    genericPartition_properlyLazy(_.partition(_ % 2 == 0))
  }

  @Test
  def partitionMap_properlyLazy(): Unit = {
    genericPartition_properlyLazy(_.partitionMap(i => if (i % 2 == 0) Left(i) else Right(i)))
  }

  @Test
  def map_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.map(_ + 1))
  }

  @Test
  def tapEach_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.tapEach(_ + 1))
  }

  @Test
  def collect_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_ collect { case i => i })
    genericLazyOp_properlyLazy(_ collect { case i if i % 2 != 0 => i }, DropProfile(dropCount = 1, repeatedDrops = true))
  }

  @Test
  def collectFirst_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ collectFirst { case i if i % 2 == 0 => i }, 1)
    assertLazyAllSkipping(_ collectFirst { case i if i % 2 != 0 => i }, 2)
  }

  @Test // scala/scala#6960
  def withFilter_withFilter_properlyLazy(): Unit = {
    assertLazyAll(_.withFilter(_ => true).withFilter(_ => true))
    genericFilter_properlyLazy(_.withFilter(_).withFilter(_ => true).map(identity), isFlipped = false)
  }

  @Test // scala/bug#9134
  def filter_map_properlyLazy(): Unit = {
    genericFilter_properlyLazy(_ filter _ map identity, isFlipped = false)
  }

  @Test // scala/bug#9134
  def withFilter_map_properlyLazy(): Unit = {
    genericFilter_properlyLazy(_ withFilter _ map identity, isFlipped = false)
  }

  private def genericFlatMap_properlyLazy(flatMapOp: (LazyList[Int], Int => IterableOnce[Int]) => LazyList[Int]): Unit = {
    val op = lazyListOp(flatMapOp(_, _ :: Nil))
    genericLazyOp_properlyLazy(op)

    // Check that calling the `flatMap` operation on instances of `LazyList` leaves them with lazy heads
    val checkers = (1 to 4) map { _ => new OpLazinessChecker }
    val lazyList = flatMapOp(LazyList.from(0).take(checkers.length), i => checkers(i).lazyList)
    for ((checker, i) <- checkers.zipWithIndex) {
      if (i > 0) lazyList.drop(i * LazinessChecker.count - 1).tail
      checker.assertAll(evaluated = false)
    }
  }

  @Test
  def flatMap_properlyLazy(): Unit = {
    genericFlatMap_properlyLazy(_ flatMap _)
  }

  @Test
  def flatten_properlyLazy(): Unit = {
    genericFlatMap_properlyLazy(_.map(_).flatten)
  }

  @Test
  def scanLeft_properlyLazy(): Unit = {
    val op = lazyListOp(_.scanLeft(0)(_ + _).tail)
    assertLazyAll(op)
    assertRepeatedlyLazy(op)
  }

  private def genericAppendedColl_properlyLazy(append: (LazyList[Int], Seq[Int]) => LazyList[Int]): Unit = {
    def check(suffix: => Seq[Int]): Unit = {
      val op = lazyListOp(append(_, suffix))
      assertRepeatedlyLazy(op)
    }

    for (coll <- Seq(Nil, 2 :: Nil, 1 to 10)) check(coll)

    // Check that appending a `LazyList` leaves it fully lazy
    val checker = new OpLazinessChecker
    val ll = append(LazyList.from(0).take(4), checker.lazyList)
    checker.assertAll(evaluated = false)
    ll.tail.tail.tail.tail // should be the appended LazyList
    checker.assertAll(evaluated = false)
  }

  private def genericAppendedCollValue_properlyLazy(append: (LazyList[Int], Seq[Int]) => LazyList[Int]): Unit = {
    genericAppendedColl_properlyLazy(append)
    val ll = LazyList.from(1)
    assertKnownEmptyYields(append(_, ll))(_ eq ll)
    assertKnownEmptyYieldsKnownEmpty(append(_, Nil))
  }

  @Test
  def lazyAppendedAll_properlyLazy(): Unit = {
    genericAppendedColl_properlyLazy(_ lazyAppendedAll _)
  }

  @Test
  def appendedAll_properlyLazy(): Unit = {
    genericAppendedCollValue_properlyLazy(_ appendedAll _)
    genericAppendedCollValue_properlyLazy(_ :++ _)
  }

  @Test
  def concat_properlyLazy(): Unit = {
    genericAppendedCollValue_properlyLazy(_ concat _)
    genericAppendedCollValue_properlyLazy(_ ++ _)
  }

  @deprecated("Uses deprecated union", since="2.13.0")
  @Test
  def union_properlyLazy(): Unit = {
    genericAppendedCollValue_properlyLazy(_ union _)
  }

  @Test
  def appended_properlyLazy(): Unit = {
    val op = lazyListOp(_ appended -1)
    assertLazyAll(op)
    assertRepeatedlyLazy(op)
  }

  @Test
  def prepended_properlyLazy(): Unit = {
    val op = lazyListOp(_.prepended(-1).tail)
    assertLazyAll(op)
    assertRepeatedlyLazy(op)
  }

  @Test
  def prependedAll_properlyLazy(): Unit = {
    def check(prefix: Seq[Int]): Unit = {
      val op = lazyListOp { ll =>
        var prepended = ll prependedAll prefix
        var toDrop = prefix.length
        while (toDrop > 0) {
          prepended = prepended.tail
          toDrop -= 1
        }
        prepended
      }
      assertLazyAll(op)
      assertRepeatedlyLazy(op)
    }
    for (coll <- Seq(Nil, 2 :: Nil, 1 to 10)) check(coll)
    assertKnownEmptyYieldsKnownEmpty(_ prependedAll Nil)

    // Check that prepending a `LazyList` leaves it fully lazy
    val checker = new OpLazinessChecker
    LazyList.from(0)
      .take(4)
      .prependedAll(checker.lazyList)
    checker.assertAll(evaluated = false)
  }

  @Test
  def drop_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.drop(2), DropProfile(dropCount = 2, repeatedDrops = false))
  }

  @Test
  def dropWhile_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.dropWhile(_ < 2), DropProfile(dropCount = 2, repeatedDrops = false))
  }

  @Test
  def dropRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.dropRight(2))
    assertLazyAll(op)
    assertRepeatedlyLazy(op, DropProfile(dropCount = 2, repeatedDrops = false))
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def take_properlyLazy(): Unit = {
    val op = lazyListOp(_.take(4))
    genericLazyOp_properlyLazy(op)
    assertLazyAllSkipping(op.thenForce, 4)
  }

  @Test
  def takeWhile_properlyLazy(): Unit = {
    val op = lazyListOp(_.takeWhile(_ < 4))
    genericLazyOp_properlyLazy(op)
    assertLazyAllSkipping(op.thenForce, 5)
  }

  @Test
  def takeRight_properlyLazy(): Unit = {
    val op = lazyListOp(_.takeRight(4))
    assertLazyAll(op)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def slice_properlyLazy(): Unit = {
    val op = lazyListOp(_.slice(2, LazinessChecker.count - 2))
    genericLazyOp_properlyLazy(op, DropProfile(dropCount = 2, repeatedDrops = false))
    assertLazyAllSkipping(op.thenForce, LazinessChecker.count - 2)
  }

  @Test
  def splitAt_properlyLazy(): Unit = {
    val split = lazyListOp(_ splitAt 4)
    genericLazyOp_properlyLazy(split.andThen(_._1))
    genericLazyOp_properlyLazy(split.andThen(_._2), DropProfile(dropCount = 4, repeatedDrops = false))
  }

  @Test
  def apply_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.apply(4), 5)
  }

  @Test
  def lengthCompare_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ lengthCompare 3, 4)
  }

  @Test
  def sizeCompare_properlyLazy(): Unit = {
    for (factory <- List[SeqFactory[Seq]](LazyList, Vector)) {
      assertLazyAllSkipping(_ sizeCompare factory.fill(3)(1), 4)
    }
  }

  @Test
  def iterator_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.iterator to LazyList)
  }

  @Test
  def view_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.view to LazyList)
  }

  @Test
  def toString_properlyLazy(): Unit = {
    assertLazyAll(_.toString())
  }

  @Test
  def contains_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ contains 0, 1)
    assertLazyAllSkipping(_ contains 3, 4)
  }

  @Test
  def containsSlice_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ containsSlice (0 to 2), 3)
    assertLazyAllSkipping(_ containsSlice (3 to 7), 8)

    // check laziness of slice when it is a `LazyList`
    val checker = new OpLazinessChecker
    assert(!LazyList.from(3).take(LazinessChecker.doubleCount).containsSlice(checker.lazyList))
    assertNotEvaluatedSkipping(checker, 1)
  }

  @Test
  def corresponds_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.corresponds(Iterator.empty[Int])(_ == _), 1)
    assertLazyAllSkipping(_.corresponds(LazyList.empty[Int])(_ == _), 1)

    assertLazyAllSkipping(_.corresponds(Iterator.from(1))(_ == _), 1)
    assertLazyAllSkipping(_.corresponds(LazyList.from(1))(_ == _), 1)

    assertLazyAllSkipping(_.corresponds(Iterator.from(0).take(1))(_ == _), 2)
    assertLazyAllSkipping(_.corresponds(LazyList.from(0).take(1))(_ == _), 2)

    // check laziness of corresponding `LazyList`
    def check(lazyList: LazyList[Int], withChecker: OpLazinessChecker => Unit): Unit = {
      val checker = new OpLazinessChecker
      assert(!lazyList.corresponds(checker.lazyList)(_ == _))
      withChecker(checker)
    }
    check(LazyList.from(1), assertNotEvaluatedSkipping(_, 1))
    check(LazyList.empty, assertNotEvaluatedSkipping(_, 1))
  }

  @Test
  def diff_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_ diff Nil)
  }

  private def genericDistinctProperlyLazy(op: LazyListToLazyListOp): Unit = {
    assertRepeatedlyLazy(op)
    assertKnownEmptyYieldsKnownEmpty(op)
  }

  @Test
  def distinct_properlyLazy(): Unit = {
    genericDistinctProperlyLazy(_.distinct)
  }

  @Test
  def distinctBy_properlyLazy(): Unit = {
    genericDistinctProperlyLazy(_.distinctBy(identity))
  }

  @Test
  def startsWith_properlyLazy(): Unit = {
    import LazinessChecker._
    assertLazyAllSkipping(_.startsWith(0 until halfCount), halfCount)
    assertLazyAllSkipping(_.startsWith(halfCount to count), 1)
  }

  @Test
  def exists_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ exists { _ < 2 }, 1)
    assertLazyAllSkipping(_ exists { _ > 2 }, 4)
  }

  @Test
  def find_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ find { _ < 2 }, 1)
    assertLazyAllSkipping(_ find { _ > 2 }, 4)
  }

  @Test
  def forall_properlyLazy(): Unit = {
    assertLazyAllSkipping(_ forall { _ > 2 }, 1)
    assertLazyAllSkipping(_ forall { _ < 2 }, 3)
  }

  @Test
  def force_properlyStrict(): Unit = {
    val checker = new OpLazinessChecker
    checker.lazyList.force
    checker.assertAll(evaluated = true)
  }

  private def genericSliding_properlyLazy(op: (LazyList[Int], Int) => Iterator[LazyList[Int]],
                                          evalExtra: Int = 0,
                                          skip: Int = 0): Unit = {
    val op3 = lazyListOp(op(_, 3))
    assertLazyAll(op3)
    assertLazyAllSkipping(op3 andThen { _.hasNext }, 1)
    assertLazyAllSkipping(op3 andThen { _.next().force }, 3)
    assertLazyAllSkipping(op3 andThen { _.drop(1).hasNext }, 4 + evalExtra + skip)
    assertLazyAllSkipping(op3 andThen { _.drop(1).next().force }, 6 + skip)
    assertKnownEmptyYields(op3)(_ eq Iterator.empty)
  }

  @Test
  def grouped_properlyLazy(): Unit = {
    genericSliding_properlyLazy(_ grouped _)
  }

  @Test
  def sliding_properlyLazy(): Unit = {
    genericSliding_properlyLazy(_ sliding _, evalExtra = 2)
    genericSliding_properlyLazy(_.sliding(_, 2), evalExtra = 1)
    genericSliding_properlyLazy(_.sliding(_, 3))
    genericSliding_properlyLazy(_.sliding(_, 4), skip = 1)
  }

  @Test
  def indexOf_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.indexOf(0), 1)
    assertLazyAllSkipping(_.indexOf(2), 3)
    assertLazyAllSkipping(_.indexOf(6, 5), 7)
  }

  @Test
  def indexOfSlice_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.indexOfSlice(0 to 5), 6)
    assertLazyAllSkipping(_.indexOfSlice(1 to 3), 4)
    assertLazyAllSkipping(_.indexOfSlice(6 to 9, 5), 10)

    // check laziness of slice when it is a `LazyList`
    val checker = new OpLazinessChecker
    assertEquals(-1, LazyList.from(3).take(LazinessChecker.doubleCount).indexOfSlice(checker.lazyList))
    assertNotEvaluatedSkipping(checker, 1)
  }

  @Test
  def indexWhere_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.indexWhere(_ < 2), 1)
    assertLazyAllSkipping(_.indexWhere(_ > 2), 4)
    assertLazyAllSkipping(_.indexWhere(_ > 2, 4), 5)
  }

  @Test
  def init_properlyLazy(): Unit = {
    val op = lazyListOp(_.init)
    assertLazyAllSkipping(op, 1)
    val d = DropProfile(dropCount = 1, repeatedDrops = false)
    assertLazyNextStateWhenStateEvaluated(op, d)
  }

  @Test
  def inits_properlyLazy(): Unit = {
    import LazinessChecker.halfCount
    val op = lazyListOp(_.inits.drop(halfCount).next())
    assertLazyAllSkipping(op, halfCount + 1)
    val d = DropProfile(dropCount = halfCount + 1, repeatedDrops = false)
    assertLazyNextStateWhenStateEvaluated(op, d)
  }

  @Test
  def tails_properlyLazy(): Unit = {
    import LazinessChecker.halfCount
    def check(op: LazyListToLazyListOp, skip: Int): Unit = {
      assertLazyAllSkipping(op, skip)
      assertLazyNextStateWhenStateEvaluated(op, DropProfile(dropCount = skip, repeatedDrops = false))
    }

    val tails = lazyListOp(_.tails)
    check(tails.andThen(_.next()), 1)
    check(tails.andThen(_.drop(halfCount).next()), halfCount + 1)
  }

  @Test
  def intersect_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_.intersect(LazinessChecker.indices))
  }

  @Test
  def padTo_properlyLazy(): Unit = {
    val op = lazyListOp(_.padTo(LazinessChecker.doubleCount, -1))
    assertLazyAll(op)
    assertRepeatedlyLazy(op)

    genericLazyOp_properlyLazy(_.padTo(0, -1))
  }

  @Test
  def patch_properlyLazy(): Unit = {
    import LazinessChecker._
    val values = halfCount :: 0 :: Nil
    for {
      from <- count :: values
      replaced <- values
    } {
      val op = lazyListOp(_.patch(from, Nil, replaced))
      val d = DropProfile(dropCount = replaced, repeatedDrops = false)
      genericLazyOp_properlyLazy(op, d)
    }
  }

  private def genericSameElements_properlyLazy(same: (Seq[Int], Seq[Int]) => Boolean): Unit = {
    assertLazyAll(ll => same(ll, ll))

    val rSame = (x: Seq[Int], y: Seq[Int]) => same(y, x)
    for (equal <- same :: rSame :: Nil) {
      assertLazyAllSkipping(equal(_, Nil), 1)
      assertLazyAllSkipping(equal(_, 1 to 10), 1)
      assertLazyAllSkipping(equal(_, 0 until 10), 11)
    }
  }

  @Test
  def sameElements_properlyLazy(): Unit = {
    genericSameElements_properlyLazy(_ sameElements _)
  }

  @Test
  def `== properlyLazy`(): Unit = {
    genericSameElements_properlyLazy(_ == _)
  }

  @Test
  def search_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.search(0), 1)
    assertLazyAllSkipping(_.search(1), 2)
    assertLazyAllSkipping(_.search(-1, 4, 7), 7)
  }

  @Test
  def segmentLength_properlyLazy(): Unit = {
    assertLazyAllSkipping(_.segmentLength(_ => false), 1)
    assertLazyAllSkipping(_.segmentLength(_ => false, 4), 5)
  }

  @Test
  def span_properlyLazy(): Unit = {
    val span = lazyListOp(_.span(_ < 4))
    val op1 = span.andThen(_._1)
    val op2 = span.andThen(_._2)

    genericLazyOp_properlyLazy(op1)
    genericLazyOp_properlyLazy(op2, DropProfile(dropCount = 4, repeatedDrops = false))

    assertLazyAllSkipping(op1.thenForce, 5)
    genericLazyOp_properlyLazy(op2.andThen(_.drop(1)), DropProfile(dropCount = 5, repeatedDrops = false))
  }

  @Test
  def to_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(_ to LazyList)
  }

  @Test
  def updated_properlyLazy(): Unit = {
    val op = lazyListOp(_.updated(1, 2))
    assertLazyAllSkipping(op, 2)
    assertLazyNextStateWhenStateEvaluated(op.andThen(_.drop(2)), DropProfile(dropCount = 2, repeatedDrops = false))
  }

  @Test
  def zip_properlyLazy(): Unit = {
    assertLazyAll(_.zip(Nil).force)
    val op = lazyListOp(_.zip(LazyList from 0))
    assertLazyAll(op)
    assertKnownEmptyYields(op)(_.knownSize == 0)
    genericLazyOp_properlyLazy(op.andThen(_.map({ case (a, b) => a + b })))
  }

  @Test
  def lazyZip_properlyLazy(): Unit = {
    assertLazyAll(_.lazyZip(Nil).to(LazyList).force)
    val op = lazyListOp(_.lazyZip(LazyList from 0))
    assertLazyAll(op)
    assertKnownEmptyYields(op)(_.knownSize == 0)

    genericLazyOp_properlyLazy(op.andThen(_.map({ case (a, b) => a + b })))
  }

  @Test
  def zipWithIndex_properlyLazy(): Unit = {
    val op = lazyListOp(_.zipWithIndex)
    assertLazyAll(op)
    assertKnownEmptyYields(op)(_.knownSize == 0)
    genericLazyOp_properlyLazy(op.andThen(_.map({ case (a, b) => a + b })))
  }

  @Test
  def zipAll_properlyLazy(): Unit = {
    val op1 = lazyListOp(_.zipAll(Nil, 0, 0))
    assertLazyAll(op1)
    assertKnownEmptyYields(op1)(_.knownSize == 0)

    val op2 = lazyListOp(_.zipAll(LazyList.tabulate(LazinessChecker.doubleCount)(i => i), 0, 0))
    for (op <- op1 :: op2 :: Nil) {
      val sum = op.andThen(_.map({ case (a, b) => a + b }))
      assertLazyAll(sum)
      assertRepeatedlyLazy(sum)
    }
  }

  @Test
  def unzip_properlyLazy(): Unit = {
    val tuple = lazyListOp(_.map(i => (i, i)).unzip)
    val op1 = tuple.andThen(_._1)
    val op2 = tuple.andThen(_._2)

    for (op <- op1 :: op2 :: Nil) {
      genericLazyOp_properlyLazy(op)
    }
  }

  @Test
  def unzip3_properlyLazy(): Unit = {
    val tuple = lazyListOp(_.map(i => (i, i, i)).unzip3)
    val op1 = tuple.andThen(_._1)
    val op2 = tuple.andThen(_._2)
    val op3 = tuple.andThen(_._3)

    for (op <- op1 :: op2 :: op3 :: Nil) {
      genericLazyOp_properlyLazy(op)
    }
  }

  @Test
  def serialization_properlyLazy(): Unit = {
    def serializeDeserialize(obj: LazyList[Int]): LazyList[Int] = {
      import java.io._
      val buffer = new ByteArrayOutputStream
      val out = new ObjectOutputStream(buffer)
      out.writeObject(obj)
      val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
      in.readObject.asInstanceOf[LazyList[Int]]
    }

    assertLazyAll(serializeDeserialize)
    val op = lazyListOp { list =>
      list.take(4).force
      serializeDeserialize(list)
    }
    assertLazyAllSkipping(op, 4)
  }

  private def genericCons_unapply_properlyLazy(unapply: LazyList[Int] => Option[(Int, LazyList[Int])]): Unit = {
    assertLazyAllSkipping(unapply, 1)
  }

  @Test
  def cons_unapply_properlyLazy(): Unit = {
    genericCons_unapply_properlyLazy(LazyList.cons.unapply)
  }

  @Test
  def `#::_unapply_properlyLazy`(): Unit = {
    genericCons_unapply_properlyLazy(LazyList.#::.unapply)
  }

  /* factory laziness tests */

  @Test
  def fromIterator_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.from(Iterator.tabulate(LazinessChecker.count) { i => init.evaluate(i); i })
    }
    assertRepeatedlyLazy(factory)
  }

  @Test
  def fromLazyList_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(LazyList from _)
  }

  @Test
  def unfold_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.unfold(0) { i =>
        if (i >= LazinessChecker.count) None
        else {
          init.evaluate(i)
          Some((i, i + 1))
        }
      }
    }
    assertRepeatedlyLazy(factory)
  }

  @Test
  def iterate_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.iterate(0) { i => init.evaluate(i); i + 1 }
    }
    assertRepeatedlyLazy(factory)
  }

  @Test
  def iterateLen_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.iterate(0, 10) { i => init.evaluate(i); i + 1 }
    }
    assertRepeatedlyLazy(factory)
  }

  @Test
  def tabulate_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      LazyList.tabulate(LazinessChecker.count) { i => init.evaluate(i); i }
    }
    assertRepeatedlyLazy(factory)
  }

  private def genericCons_properlyLazy(cons: (=> Int, => LazyList[Int]) => LazyList[Int]): Unit = {
    val headInitFactory = lazyListFactory { init =>
      def gen(index: Int): LazyList[Int] = {
        def elem(): Int = { init.evaluate(index); index }
        if (index >= LazinessChecker.count) LazyList.empty
        else cons(elem(), gen(index + 1))
      }

      gen(0)
    }
    assertRepeatedlyLazy(headInitFactory)

    val tailInitFactory = lazyListFactory { init =>
      def gen(index: Int): LazyList[Int] = {
        if (index >= LazinessChecker.count) LazyList.empty
        else {
          init.evaluate(index)
          cons(index, gen(index + 1))
        }
      }

      LazyList.empty lazyAppendedAll gen(0) // prevent initial state evaluation
    }
    assertRepeatedlyLazy(tailInitFactory)
  }

  @Test
  def cons_properlyLazy(): Unit = {
    genericCons_properlyLazy(LazyList.cons(_, _))
  }

  @Test
  def `#::_properlyLazy`(): Unit = {
    genericCons_properlyLazy(_ #:: _)
  }

  @Test
  def `#:::_properlyLazy`(): Unit = {
    val headInitFactory = lazyListFactory { init =>
      def gen(index: Int): LazyList[Int] = {
        def elem(): LazyList[Int] = LazyList.fill(1) { init.evaluate(index); index }
        if (index >= LazinessChecker.count) LazyList.empty
        else elem() #::: gen(index + 1)
      }

      gen(0)
    }
    assertRepeatedlyLazy(headInitFactory)

    val tailInitFactory = lazyListFactory { init =>
      def gen(index: Int): LazyList[Int] = {
        def elem(): LazyList[Int] = { init.evaluate(index); LazyList.fill(1)(index) }
        if (index >= LazinessChecker.count) LazyList.empty
        else elem() #::: gen(index + 1)
      }

      gen(0)
    }
    assertRepeatedlyLazy(tailInitFactory)
  }

  @Test
  def fill_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      var counter = 0
      LazyList.fill(10) {
        init.evaluate(counter)
        counter += 1
        counter
      }
    }
    assertRepeatedlyLazy(factory)
  }

  @Test
  def continually_properlyLazy(): Unit = {
    val factory = lazyListFactory { init =>
      var counter = 0
      LazyList.continually {
        init.evaluate(counter)
        counter += 1
        counter
      }
    }
    assertRepeatedlyLazy(factory)
  }

  @Test
  def apply_companion_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(LazyList(_: _*))
  }

  @Test
  def concat_companion_properlyLazy(): Unit = {
    val quarterCount = LazinessChecker.count / 4
    assertEquals(0, LazyList.concat().knownSize)

    val factory = lazyListFactory { init =>
      val lists = (0 until 4) map { i =>
        LazyList.tabulate(quarterCount) { j =>
          val index = i * 4 + j
          init.evaluate(index)
          index
        }
      }
      LazyList.concat(Nil ++: lists: _*)
    }

    assertRepeatedlyLazy(factory)
  }

  // TODO: fix this test after impl is made lazier
  @Test
  def range_properlyLazy(): Unit = {
    var counter = 0
    case class CustomLong(value: Long) {
      counter += 1
    }
    object CustomLong {
      import scala.language.implicitConversions
      implicit def long2CustomLong(long: Long): CustomLong = CustomLong(long)

      implicit val customIntegralIsIntegral: Integral[CustomLong] = new Integral[CustomLong] {
        private val I = Integral[Long]

        override def quot(x: CustomLong, y: CustomLong) = I.quot(x.value, y.value)
        override def rem(x: CustomLong, y: CustomLong) = I.rem(x.value, y.value)
        override def plus(x: CustomLong, y: CustomLong) = I.plus(x.value, y.value)
        override def minus(x: CustomLong, y: CustomLong) = I.minus(x.value, y.value)
        override def times(x: CustomLong, y: CustomLong) = I.times(x.value, y.value)
        override def negate(x: CustomLong) = I.negate(x.value)
        override def fromInt(x: Int) = I.fromInt(x)
        override def parseString(str: String) = I.parseString(str).map(CustomLong.apply)
        override def toInt(x: CustomLong) = I.toInt(x.value)
        override def toLong(x: CustomLong) = I.toLong(x.value)
        override def toFloat(x: CustomLong) = I.toFloat(x.value)
        override def toDouble(x: CustomLong) = I.toDouble(x.value)
        override def compare(x: CustomLong, y: CustomLong) = I.compare(x.value, y.value)
      }
    }

    def checkRange(ll: => LazyList[CustomLong]): Unit = {
      counter = 0
      ll
      assert(counter < 10) // TODO: be much more precise
    }

    checkRange(LazyList.range(CustomLong(0), CustomLong(1000)))
    checkRange(LazyList.range(CustomLong(0), CustomLong(1000), CustomLong(2)))
    checkRange(LazyList.range(CustomLong(0), CustomLong(1000), CustomLong(-2)))
  }

  @Test
  def unapplySeq_properlyLazy(): Unit = {
    genericLazyOp_properlyLazy(LazyList.unapplySeq(_).toSeq.to(LazyList))
  }

  @Test
  def builder_properlyLazy(): Unit = {
    val op = lazyListOp { ll => (LazyList.newBuilder[Int] ++= ll).result() }
    assertLazyAll(op)
    assertRepeatedlyLazy(op)
  }
}

private object LazyListLazinessTest {
  /* core laziness utilities */

  /** Note: not reusable. */
  sealed abstract class LazinessChecker extends Serializable {
    import LazinessChecker._

    protected[this] val states = new Array[Boolean](count)

    protected[this] def internalCheckIndex(index: Int): Unit =
      if (index < 0 || index >= count) {
        throw new IllegalStateException("internal failure - bad index: " + index)
      }

    protected[this] def checkIndex(index: Int): Unit = {
      if (index < 0 || index >= count) throw new IndexOutOfBoundsException(index.toString)
    }

    private[this] def assertUnchecked(evaluated: Boolean,
                                      index: Int,
                                      ex: ExceptionProvider): Unit = {
      if (states(index) != evaluated) {
        throw ex(s"state($index) was ${if (evaluated) "not " else ""}evaluated")
      }
    }

    private[this] def assertCheckedInternal(evaluated: Boolean,
                                            index: Int,
                                            ex: ExceptionProvider): Unit = {
      internalCheckIndex(index)
      assertUnchecked(evaluated, index, ex)
    }

    private[this] def assertChecked(evaluated: Boolean,
                                    index: Int,
                                    ex: ExceptionProvider): Unit = {
      checkIndex(index)
      assertUnchecked(evaluated, index, ex)
    }

    final def assert(evaluated: Boolean, index: Int): Unit =
      assertChecked(evaluated, index, defaultException)

    private[this] def assertAllImpl(evaluated: Boolean,
                                    ex: ExceptionProvider,
                                    skip: Int): Unit = {
      require(skip >= 0, "`skip` cannot be negative")
      require(skip < count, s"`skip` ($skip) >= size of lazy list ($count) - will not assert anything")
      for (i <- skip until count) assertCheckedInternal(evaluated, i, ex)
    }

    /** Asserts that the evaluated status of all states matches the one specified.
      *
      * @param evaluated whether or not all states should be evaluated
      * @param ex        an exception generator (creates an `AssertionError` by default)
      */
    final def assertAll(evaluated: Boolean, ex: ExceptionProvider = defaultException): Unit =
      assertAllImpl(evaluated, ex, 0)

    /** Asserts that the evaluated status of all states except for the first
      * `skip` ones matches the one specified.
      *
      * @param evaluated whether or not all except the first `skip` states should be evaluated
      * @param skip      the number of states not to check
      */
    final def assertAllSkipping(evaluated: Boolean, skip: Int): Unit =
      assertAllImpl(evaluated, defaultException, skip)

    // for debugging
    final override def toString: String = {
      val sb = new java.lang.StringBuilder(getClass.getSimpleName).append("(")
      for (i <- 0 until 8) { sb.append(s"state($i): ${states.array(i)}, ") }
      sb.append("...)")
      sb.toString
    }
  }

  object LazinessChecker {
    type ExceptionProvider = String => Throwable

    final val count = 16
    final val halfCount = count / 2
    final val doubleCount = count * 2
    final val indices: Range = 0 until count

    private val defaultException: ExceptionProvider = new AssertionError(_)
  }

  /* op laziness utilities */

  /** Note: not reusable. */
  final class OpLazinessChecker extends LazinessChecker {
    import LazinessChecker.count

    // prevent recursive serialization - that would lead to eventually
    // attempting to deserialize the proxy, which is wrong
    @transient val lazyList: LazyList[Int] = LazyList.tabulate(count) { index =>
      internalCheckIndex(index)
      states(index) = true
      index
    }
  }

  final case class DropProfile(dropCount: Int, repeatedDrops: Boolean) {
    require(dropCount < LazinessChecker.count,
      s"dropCount=$dropCount >= size of lazy list (${LazinessChecker.count})")
    def apply(iteration: Int): Int =
      if (iteration <= 0) 0
      else if (repeatedDrops) dropCount * iteration
      else dropCount
  }

  final val NoDrops = DropProfile(dropCount = 0, repeatedDrops = false)

  type LazyListOp[U] = LazyList[Int] => U
  type LazyListToLazyListOp = LazyListOp[LazyList[Int]]

  implicit final class RichLazyListToLazyListOp(private val self: LazyListToLazyListOp) extends AnyVal {
    def thenForce: LazyListToLazyListOp = self.andThen(_.force)
  }

  // save on having to write type annotations all the time
  def lazyListOp[U](op: LazyListOp[U]): LazyListOp[U] = op

  /** Asserts that the operation does not evaluate the initial lazy list or
    * subsequent tails at all before methods are invoked on them.
    */
  def assertRepeatedlyLazy(op: LazyListToLazyListOp, d: DropProfile = NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    checker.assert(evaluated = false, 0)
    result.tail
    checker.assert(evaluated = false, 1 + d(1))
    result.tail.tail
    checker.assert(evaluated = false, 2 + d(2))
  }

  /** Asserts that the operation does not evaluate the next state of the lazy list
    * when the head is evaluated.
    *
    * Note: this is a subset of [[assertRepeatedlyLazy]].
    */
  def assertLazyNextStateWhenStateEvaluated(op: LazyListToLazyListOp, d: DropProfile = NoDrops): Unit = {
    val checker = new OpLazinessChecker
    val result = op(checker.lazyList)
    result.isEmpty
    checker.assert(evaluated = false, 1 + d(1))
    result.tail.isEmpty
    checker.assert(evaluated = false, 2 + d(2))
  }

  /** Asserts that the operation does not evaluate any states or heads. */
  def assertLazyAll[U](op: LazyListOp[U]): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    checker.assertAll(evaluated = false)
  }

  /** Asserts that the checker does not have any heads or states evaluated
    * other than the first `skip`.
    */
  def assertNotEvaluatedSkipping(checker: OpLazinessChecker, skip: Int): Unit = {
    checker.assertAllSkipping(evaluated = false, skip = skip)
  }

  /** Asserts that the operation does not evaluate any heads or states
    * other than the first `skip`.
    */
  def assertLazyAllSkipping[U](op: LazyListOp[U], skip: Int): Unit = {
    val checker = new OpLazinessChecker
    op(checker.lazyList)
    assertNotEvaluatedSkipping(checker, skip)
  }

  /** Asserts that a predicate holds when a given operation is performed on
    * a lazy list that is known to be empty.
    */
  def assertKnownEmptyYields[A](op: LazyListOp[A])(predicate: A => Boolean): Unit = {
    assert(predicate(op(LazyList.empty)))
  }

  /** Asserts that operation yields a lazy list that is known to be empty
    * when performed on a lazy list that is known to be empty.
    */
  def assertKnownEmptyYieldsKnownEmpty(op: LazyListToLazyListOp): Unit =
    assertKnownEmptyYields(op)(_.knownSize == 0)

  /* factory laziness utilities */

  /** Note: not reusable.
    *
    * racy, but not being used in a concurrent environment
    */
  final class FactoryLazinessChecker extends LazinessChecker {
    private[this] var ll: LazyList[Int] = _

    final class Initializer private[FactoryLazinessChecker] {
      /** Marks state evaluated for a given index. */
      def evaluate(index: Int): Unit = {
        checkIndex(index)
        if (states(index)) throw new IllegalStateException(s"Can only evaluate state($index) once")
        states(index) = true
      }
    }

    def initialize(init: Initializer => LazyList[Int]): this.type = {
      if (ll ne null) throw new IllegalStateException("already initialized")
      val res = init(new Initializer)
      if (res eq null) throw new NullPointerException("null LazyList")
      ll = res
      this
    }

    def lazyList: LazyList[Int] = {
      if (ll eq null) throw new IllegalStateException("not initialized")
      ll
    }
  }

  object FactoryLazinessChecker {
    type Factory = FactoryLazinessChecker#Initializer => LazyList[Int]
  }

  def lazyListFactory(factory: FactoryLazinessChecker.Factory): FactoryLazinessChecker.Factory = factory

  def assertRepeatedlyLazy(factory: FactoryLazinessChecker.Factory): Unit = {
    val checker = new FactoryLazinessChecker().initialize(factory)
    checker.assert(evaluated = false, 0)
    checker.lazyList.head
    checker.assert(evaluated = false, 1)
    checker.lazyList.tail.head
    checker.assert(evaluated = false, 2)
  }
}
