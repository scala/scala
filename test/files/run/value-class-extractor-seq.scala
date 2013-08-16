import scala.runtime.ScalaRunTime.stringOf

final class ArrayOpt[T](val xs: Array[T]) extends AnyVal {
  def isEmpty = xs == null
  def get = xs
}

object Bip {
  def mkInts(xs: Array[Short]) = xs map (_.toInt)
  def unapplySeq(x: Any): ArrayOpt[Int] = x match {
    case xs: Array[Int]   => new ArrayOpt(xs)
    case xs: Array[Short] => new ArrayOpt(mkInts(xs))
    case _                => new ArrayOpt(null)
  }
  // public int[] unapplySeq(java.lang.Object);
  //      0: aload_1
  //      1: astore_2
  //      2: aload_2
  //      3: instanceof    #52                 // class "[I"
  //      6: ifeq          20
  //      9: aload_2
  //     10: checkcast     #52                 // class "[I"
  //     13: astore_3
  //     14: aload_3
  //     15: astore        4
  //     17: goto          47
  //     20: aload_2
  //     21: instanceof    #58                 // class "[S"
  //     24: ifeq          44
  //     27: aload_2
  //     28: checkcast     #58                 // class "[S"
  //     31: astore        5
  //     33: aload_0
  //     34: aload         5
  //     36: invokevirtual #60                 // Method mkInts:([S)[I
  //     39: astore        4
  //     41: goto          47
  //     44: aconst_null
  //     45: astore        4
  //     47: aload         4
  //     49: areturn
}

object Test {
  def f(x: Any) = x match {
    case Bip(a, b, c)      => s"Bip($a, $b, $c)"
    case Bip(a, b, c @ _*) => s"Bip($a, $b, c @ ${stringOf(c)}: _*)"
    case _                 => "" + x.getClass
  }

  def main(args: Array[String]): Unit = {
    println(f(Array[Int](1,2,3)))
    println(f(Array[Int](1,2,3,4,5)))
    println(f(Array[Int](1)))
  }
  // Bip(1, 2, 3)
  // Bip(1, 2, c @ [I@782be20e: _*)
  // class [I
}
