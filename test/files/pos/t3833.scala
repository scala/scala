object Main {
  def mkArray[T <: A](atype: Int) :T#AType = {
    (atype match {
      case 1 =>
        new Array[Int](10)
        // Decompiled code: return (Object[])new int[10];
      case 2 =>
        new Array[Float](10)
    }).asInstanceOf[T#AType]
  }

  def main(args: Array[String]) {
    println(mkArray[I](1))
    //java.lang.ClassCastException: [I cannot be cast to [Ljava.lang.Object;
  }
}

trait A {
  type AType <: AnyRef
}
trait I extends A {
  type AType = Array[Int]
}
trait F extends A {
  type AType = Array[Float]
}
