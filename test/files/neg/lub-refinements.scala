// Members of the lub base type that are refined in every lubbed type become refinements of the lub
object Test {
  trait A { def f: Any; def g: Any; type T }
  class B extends A { def f: Int = 1; def g: Any = 1; type T = Int; def b = 1 }
  class C extends A { def f: Int = 2; def g: Any = 2; type T = Int; def c = 2 }
  def refined(c: Boolean): Unit = {
    val x = if (c) new B else new C
    x: Nothing
  }

  // Overriding members don't match as seen from an existential lub base type
  def existential(a: Array[_]): Unit = {
    import scala.collection.mutable.ArraySeq
    val x = a match {
      case a: Array[AnyRef]  => new ArraySeq.ofRef(a)
      case a: Array[Int]     => new ArraySeq.ofInt(a)
      case a: Array[Double]  => new ArraySeq.ofDouble(a)
      case a: Array[Long]    => new ArraySeq.ofLong(a)
      case a: Array[Float]   => new ArraySeq.ofFloat(a)
      case a: Array[Char]    => new ArraySeq.ofChar(a)
      case a: Array[Byte]    => new ArraySeq.ofByte(a)
      case a: Array[Short]   => new ArraySeq.ofShort(a)
      case a: Array[Boolean] => new ArraySeq.ofBoolean(a)
      case a: Array[Unit]    => new ArraySeq.ofUnit(a)
    }
    x: Nothing
  }

  // Shared base classes linearized in a different order
  trait P { def m: Any }
  trait X extends P { override def m: Any = 1 }
  trait Y extends P { override def m: Any = 2 }
  class XY extends X with Y { override def m: Int = 3 }
  class YX extends Y with X { override def m: Int = 4 }
  def linearization(c: Boolean): Unit = {
    val x = if (c) new XY else new YX
    x: Nothing
  }
}
