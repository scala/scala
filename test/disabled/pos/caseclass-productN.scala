object Test {
  class A
  class B extends A
  class C extends B
  
  case class Bippy[T](x: Int, y: List[T], z: T) { }
  case class Bippy2[T](x: Int, y: List[T], z: T) { }
  
  def bippies = List(
    Bippy(5, List(new C), new B),
    Bippy2(5, List(new B), new C)
  )
  
  def bmethod(x: B) = ()
  
  def main(args: Array[String]): Unit = {
    bippies flatMap (_._2) foreach bmethod
    bippies map (_._3) foreach bmethod
  }
}
