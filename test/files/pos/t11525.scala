//> using options -Ystop-after:refchecks -Ydebug -uniqid
package java.lang

/* This is a pretty random test that very indirectly tests `unique`ing of `ObjectTpeJavaRef`
It's minimize from scala-js, where CI chanced on a compilation order that would first
unique `TypeBounds(lo, ObjectTpe)`, and then `TypeBounds(lo, ObjectTpeJava)`,
which would result in a Java reference to Object being replaced by one that is used
to represent a Scala occurrence of a reference to Object, which is distinct from Any.
When Java code refers to Object, it's taken as the same thing as Any, at least when
it comes to =:= and `... <:< Object-in-java`.
*/
import java.util.Iterator

class Class[A](o: Object)

class Comparable[A] { def compareTo(o: A): scala.Int = ??? }

object System {
  def currentTimeMillis(): scala.Long = ???

  def arraycopy(src: Object, srcPos: scala.Int, dest: Object, destPos: scala.Int, length: scala.Int): Unit = {
    import scala.{Boolean, Double}

    def mismatch(): Nothing =
      throw new ArrayStoreException("Incompatible array types")

    def copyPrim[@specialized T](src: Array[T], dest: Array[T]): Unit = {
        var i = length-1
        while (i >= 0) {
          dest(i+destPos) = src(i+srcPos)
          i -= 1
        }
    }

    def copyRef(src: Array[AnyRef], dest: Array[AnyRef]): Unit = {
      val x = (src.length, dest.length)

      var i = length-1
      while (i >= 0) {
        dest(i+destPos) = src(i+srcPos)
        i -= 1
      }
    }

    (src match {
      case src: Array[Boolean] =>
        dest match {
          case dest: Array[Boolean] => copyPrim(src, dest)
          case _                    => mismatch()
        }

    })
  }

  def identityHashCode(x: Object): scala.Int = {
    x.getClass
    1
  }
}

trait Iterable[T] {
  def iterator():  java.util.Iterator[T]
}
