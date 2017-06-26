package fix

import scala.{ List => _, Nil => _, Seq => _, Vector => _, :: => _, #:: => _ }
import scala.Predef.{ Map => _ }
import strawman.collection.immutable.{ LazyList, List, Map, Nil, Seq, Vector, :: }
import strawman.collection.immutable.LazyList.#::

object Collectionstrawman_v0_List {
  List(1, 2, 3)
  1 :: 2 :: 3 :: Nil
  val isEmpty: List[_] => Boolean = {
    case Nil     => true
    case x :: xs => false
  }
}

object Collectionstrawman_v0_Stream {
  LazyList(1, 2, 3)
  1 #:: 2 #:: 3 #:: LazyList.Empty
  val isEmpty: Stream[_] => Boolean = {
    case Stream.Empty => true
    case x #:: xs     => false
  }
}

object Collectionstrawman_v0_Vector {
  val xs: Vector[Int] = Vector(1, 2, 3)
}

object Collectionstrawman_v0_Seq {
  val xs: Seq[Int] = Seq(1, 2, 3)
}

object Collectionstrawman_v0_Map {
  val xs: Map[Int, String] = Map(1 -> "1", 2 -> "2", 3 -> "3")
  import strawman.collection.immutable.HashMap
  val ys = HashMap.empty
}

object Collectionstrawman_v0_ArrayBuffer {
  import strawman.collection.mutable.ArrayBuffer
  val xs: ArrayBuffer[Int] = ArrayBuffer(1, 2, 3)
}
