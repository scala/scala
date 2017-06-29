package fix

import scala.{ List => _, Nil => _, Seq => _, Vector => _, :: => _, #:: => _ }
import scala.Predef.{ Map => _, augmentString => _, intArrayOps => _, ArrowAssoc, charWrapper}
import strawman.collection.{ stringToStringOps, arrayToArrayOps }
import strawman.collection.immutable.{ LazyList, List, Map, Nil, Range, Seq, :: }
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
  val isEmpty: LazyList[_] => Boolean = {
    case LazyList.Empty => true
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

object Collectionstrawman_v0_ArrayAndString {
  def foo(xs: Array[Int], ys: String): Unit = {
    xs.map(x => x + 1)
    ys.map(c => c.toUpper)
  }
}

object Collectionstrawman_v0_Range {
  for (i <- Range.inclusive(1, 10); j <- Range(0, 10)) yield (i, j)
}
