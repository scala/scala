class Foo

abstract class C {
  val f: Foo
  def g1 = (f == f)
}
object O1 extends C {
  val f = new Foo()
  def g2 = (f == f)
}
object O2 extends C {
  object f extends Foo
  def g2 = (f == f)
}

class O3 extends C {
  object f extends Foo
  def g2 = (f == f)
}


object Test {
  def main(args: Array[String]): Unit = {
    println(O1.g1)
    println(O1.g2)
    
    println(O2.g1)
    println(O2.g2)
  
    val o3 = new O3()
    println(o3.g1)
    println(o3.g2)

  }
}
