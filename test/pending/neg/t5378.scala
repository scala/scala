import language.reflectiveCalls

class Coll[+T] {
  def contains = new { def apply[T1 <: T](value: T1) = ??? }
}

object Test {
  def main(args: Array[String]): Unit = {
    val xs = new Coll[List[String]]
    val ys: Coll[Traversable[String]] = xs
    
    println(ys contains Nil)
    // java.lang.NoSuchMethodException: Coll$$anon$1.apply(scala.collection.Traversable)
    //  at java.lang.Class.getMethod(Class.java:1605)
    //  at Test$.reflMethod$Method1(a.scala:14)
    //  at Test$.main(a.scala:14)
    //  at Test.main(a.scala)
  }
}
