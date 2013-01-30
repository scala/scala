import scala.language.reflectiveCalls

class Coll[+T] {
  type A1 <: T
  type A2 <: A1

  def contains = new { def apply[T1 <: T](value: T1) = ??? }
  def contains1 = new { def apply[T1 <: A1](value: T1) = ??? }
  def contains2 = new { def apply[T1 <: A2](value: T1) = ??? }
  def contains3 = {
    trait Bippy {
      type B1 <: T
      type B2 <: B1
    }
    new Bippy { def apply[T1 <: T](value: T1) = ??? }
    new Bippy { def apply[T1 <: B1](value: T1) = ??? }
    new Bippy { def apply[T1 <: B2](value: T1) = ??? }
    new Bippy {
      type B3 = B2
      type B4 = List[B2]
      def apply1[T1 <: B3](value: T1) = ???
      def apply2[T1 <: B4](value: T1) = ???
      def apply3(value: B3) = ???
      def apply4(value: B4) = value.head
    }
  }
  def contains4 = new {
    def apply1(s: String)(x: Int)(value: T) = ???
    def apply2[T1 <: T](s: String)(x: Int)(value: T1) = ???
  }
  def containsOk = {
    trait Bippy {
      type B1 <: AnyRef
      type B2 <: B1
    }
    new Bippy { def apply[T1 <: AnyRef](value: T1) = ??? }
    new Bippy { type B1 = String ; def apply[T1 <: B1](value: T1) = ??? }
    new Bippy { type B2 = String ; def apply[T1 <: B2](value: T1) = ??? }
  }
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
