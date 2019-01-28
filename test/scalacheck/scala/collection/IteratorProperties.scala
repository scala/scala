package scala.collection

import org.scalacheck.{Arbitrary, Gen, Properties, Prop}
import org.scalacheck.Prop.{forAll, BooleanOperators, all}

object IteratorProperties extends Properties("Iterator") {

  val smallInteger = Gen.choose(0,50)

  class SimpleIterable[A](underlying: Iterable[A]) extends Iterable[A] {
    def iterator: Iterator[A] = {
      val it = underlying.iterator
      new Iterator[A] {
        override def hasNext: Boolean = it.hasNext
        override def next(): A = it.next()
      }
    }
  }

  property("take") = check(_ take _)
  property("takeRight") = check((it, n) => it match {
    case it: Iterable[Int] => it.takeRight(n)
    case it: Iterator[Int] => View.takeRightIterator(it, n)
  })
  property("drop") = check(_ drop _)
  property("dropRight") = check((it, n) => it match {
    case it: Iterable[Int] => it.dropRight(n)
    case it: Iterator[Int] => View.dropRightIterator(it, n)
  })

  def check(f: (IterableOnceOps[Int, IterableOnce, IterableOnce[Int]], Int) => IterableOnce[Int]): Prop = forAll(Arbitrary.arbitrary[Seq[Int]], smallInteger) { (s: Seq[Int], n: Int) =>
    val indexed = s.toIndexedSeq // IndexedSeqs and their Iterators have a knownSize
    val simple = new SimpleIterable(s) // SimpleIterable and its Iterator don't
    val stream = LazyList.from(s) // Lazy
    val indexed1 = f(indexed, n).toSeq
    val indexed2 = f(indexed.iterator, n).toSeq
    val simple1 = f(simple, n).toSeq
    val simple2 = f(simple.iterator, n).toSeq
    val stream1 = f(stream, n).toSeq
    val stream2 = f(stream.iterator, n).toSeq
    (indexed1 == indexed2) :| s"indexed: $indexed1 != $indexed2" &&
      (simple1 == simple2) :| s"simple: $simple1 != $simple2" &&
      (stream1 == stream2) :| s"stream: $stream1 != $stream2" &&
      (simple1 == indexed2) :| s"simple vs indexed: $simple1 != $indexed2" &&
      (stream1 == indexed2) :| s"stream vs indexed: $stream1 != $indexed2"
  }
}
