class C extends A{

  val a = 10
  //object bb
  lazy val bb = 17
  val b = 12
}

abstract class A{
    val a: Int
    val b: Int
    val c: Int = 12
}

class B extends A{

  val a = 10
  //object bb
  lazy val bb = 17
  val b = 12
}

trait T {
    private final val a = false
}

class Impl extends T


object Test {
    def main(args: Array[String]) {
        println(new B().bb)
        println(new C().bb)
    }
}

