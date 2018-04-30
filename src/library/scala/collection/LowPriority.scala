package scala.collection

class LowPriority {
  import scala.language.implicitConversions

  /** Convert array to WrappedArray. Lower priority than ArrayOps */
  def arrayToWrappedArray[T](xs: Array[T]): mutable.IndexedSeq[T] = mutable.WrappedArray.make(xs)

  /** Convert String to Seq. Lower priority than StringOps */
  def stringToSeq(s: String): immutable.WrappedString = new immutable.WrappedString(s)
}
