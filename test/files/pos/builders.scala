object builders {

  trait Builder[-From, +To, -Elem] {
    def += (elem: Elem)
    def result: To
  }

  implicit def iterableBuilder[A, B] = new Builder[Iterable[A], Iterable[B], B] {
    println("new iterable builder")
    private val buf = new scala.collection.mutable.ListBuffer[B]
    def += (elem: B) { buf += elem }
    def result: Iterable[B] = buf.toList
  }

  implicit def listBuilder[A, B] = new Builder[List[A], List[B], B] {
    println("new list builder")
    private val buf = new scala.collection.mutable.ListBuffer[B]
    def += (elem: B) { buf += elem }
    def result: List[B] = buf.toList
  }
/*  
  def fill[A, Dim1, Dim2, Coll](n1: Int, n2: Int, elem: A)(implicit b1: Builder[Coll, Dim1, A], b2: Builder[Coll, Dim2, Dim1]) = {
    for (i <- 0 until n1) {
      for (j <- 0 until n2) {
        b1 += elem
      }	
      b2 += b1.result
    }
    b2.result
  }	
*/
/*
  implicit def arrayBuilder[A, B] = new Builder[Array[A], Array[B], B] {
    println("new arr ay builder")
    private val buf = new scala.collection.mutable.ListBuffer[B]
    def += (elem: B) { buf += elem }
    def result: Array[B] = buf.toArray
  }	
*/
  class Iter[A, C](elems: List[A]) {
    def ++ [B  >: A, D](xs: Iterable[B])(implicit b: Builder[C, D, B]): D = {
      for (x <- elems) b += x
      for (x <- xs) b += x
      b.result
    }
    def map[B, D](f: A => B)(implicit b: Builder[C, D, B]): D = {
      for (x <- elems) b += f(x)
      b.result
    }
  }
  
  def main(args : Array[String]) : Unit = {
    val x1 = new Iter[Int, List[Int]](List(1, 2, 3))
//    val x2 = new Iter[Int, Array[Int]](List(1, 2, 3))
    val x3 = new Iter[Int, Iterable[Int]](List(1, 2, 3))
    val y1: List[Int] = x1.map (_ + 1)
//    val y2: Array[Int] = x2.map (_ + 1)
    val y3: Iterable[Int] = x3.map (_ + 1)
    val z1: List[Int] = y1
//    val z2: Array[Int] = y2
    val z3: Iterable[Int] = y3
    println(z1)
//    println(z2)
    println(z3)
  }
}
