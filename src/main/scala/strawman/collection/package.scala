package strawman

import scala.{Any, AnyVal, Array, Char, IllegalArgumentException, IndexOutOfBoundsException, Int, NoSuchElementException, Unit, UnsupportedOperationException}
import scala.Predef.String
import scala.reflect.ClassTag

package object collection extends LowPriority {
  import scala.language.implicitConversions
  // ------------------ Decorators to add collection ops to existing types -----------------------

  /** Decorator to add collection operations to strings. */
  implicit def stringToStringOps(s: String): StringOps = new StringOps(s)

  /** Decorator to add collection operations to arrays. */
  implicit def arrayToArrayOps[A](as: Array[A]): ArrayOps[A] = new ArrayOps[A](as)

  implicit class toNewIterator[A](val it: scala.Iterator[A]) extends AnyVal {
    def toStrawman = new strawman.collection.Iterator[A] {
      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  implicit class toOldIterator[A](val it: strawman.collection.Iterator[A]) extends AnyVal {
    def toClassic = new scala.Iterator[A] {
      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  implicit class toNewSeq[A](val s: scala.collection.Seq[A]) extends AnyVal {
    def toStrawman: strawman.collection.Seq[A] =
      new strawman.collection.mutable.ArrayBuffer() ++= s.iterator.toStrawman
  }

  implicit class toOldSeq[A](val s: strawman.collection.Seq[A]) extends AnyVal {
    def toClassic: scala.collection.Seq[A] =
      new scala.collection.mutable.ArrayBuffer ++= s.iterator().toClassic
  }

  /** Needed to circumvent a difficulty between dotty and scalac concerning
   *  the right top type for a type parameter of kind * -> *.
   *  In Scalac, we can provide `Any`, as `Any` is kind-polymorphic. In dotty this is not allowed.
   *  In dotty, we can provide `[X] => Any`. But Scalac does not know lambda syntax.
   */
  type AnyConstr[X] = Any

  /** Collection internal utility functions.
    */
  private[collection] object DebugUtils {
    def unsupported(msg: String)     = throw new UnsupportedOperationException(msg)
    def noSuchElement(msg: String)   = throw new NoSuchElementException(msg)
    def indexOutOfBounds(index: Int) = throw new IndexOutOfBoundsException(index.toString)
    def illegalArgument(msg: String) = throw new IllegalArgumentException(msg)

    def buildString(closure: (Any => Unit) => Unit): String = {
      val output = new collection.mutable.StringBuilder
      closure { any =>
        output ++= any.toString
        output += '\n'
      }

      output.result()
    }

    def arrayString[T](array: Array[T], from: Int, until: Int): String = {
      array.slice(from, until) map ({
        case null => "n/a"
        case x    => "" + x
      }: scala.PartialFunction[T, String]) mkString " | "
    }
  }

}

class LowPriority {
  import scala.language.implicitConversions
  import strawman.collection._

  /** Convert array to iterable via view. Lower priority than ArrayOps */
  implicit def arrayToView[T](xs: Array[T]): ArrayView[T] = new ArrayView[T](xs)

  /** Convert string to iterable via view. Lower priority than StringOps */
  implicit def stringToView(s: String): StringView = new StringView(s)
}
