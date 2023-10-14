// skalac: -Vdebug -Vlog:_ -Vprint:erasure

class A {
  def compareTo(o: Any): Int = 0
}
class B extends A with Comparable[B] {
  def compareTo(b: B): Int = 0
}
object C {
  def main(args: Array[String]): Unit = {
    println(new B().compareTo(new Object()))
  }
}

/*
java.lang.ClassCastException: class java.lang.Object cannot be cast to class B (java.lang.Object is in module java.base of loader 'bootstrap'; B is in unnamed module of loader java.net.URLClassLoader @3af17be2)
        at B.compareTo(t9912.scala:5)
        at Main$.main(t9912.scala:10)
        at Main.main(t9912.scala)
*/
