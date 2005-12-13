class C[T](x: T) { override def toString(): String = x.toString(); }

object TestPolymorphicClasses {
  def main(args: Array[String]): Unit = {
    Console.println("===== polymorphic classes");
    Console.println(new C("true"));
    Console.println(new C(true));
    Console.println((new C("a")).isInstanceOf[C[String]]);
    Console.println(!(new C(42)).isInstanceOf[C[String]]);
  }
}

class Outer {
  class Inner;
}

class SubOuter extends Outer {
  class SubInner extends Inner;
}

object TestNestedClasses {
  def main(args: Array[String]): Unit = {
    Console.println("===== nested classes");
    val o1 = new Outer;
    val i1 = new o1.Inner;
    val o2 = new Outer;
    val i2 = new o2.Inner;
    Console.println( i1.isInstanceOf[o1.Inner]);
    Console.println( i2.isInstanceOf[o2.Inner]);
    Console.println(!i1.isInstanceOf[o2.Inner]);
    Console.println(!i2.isInstanceOf[o1.Inner]);
    val so1 = new SubOuter;
    val si1 = new so1.SubInner;
    val i3  = new so1.Inner;
    Console.println( si1.isInstanceOf[so1.Inner]);
    Console.println(!si1.isInstanceOf[o1.Inner]);
    Console.println(! i3.isInstanceOf[so1.SubInner]);
//    Console.println(  i3.isInstanceOf[Outer#SubInner]);
//    Console.println(  i3.isInstanceOf[Outer#Inner]);
  }
}

object TestClassesInsideFunctions {
  def f(x: String): ScalaObject = {
    class C { override def toString() = x; };
    new C
  }
  def main(args: Array[String]): Unit = {
    Console.println("===== classes nested inside functions");
    val c1 = f("true");
    val c2 = f("true");
    Console.println(c1);
    Console.println(c2);
    Console.println(!c1.getScalaType().isSameType(c2.getScalaType()));
  }
}

class Invariant[T];
class Covariant[+T];
class Contravariant[-T];

class MultiVariant[T1, -T2, +T3, T4, -T5, +T6];

object TestVariance {
  def main(args: Array[String]): Unit = {
    Console.println("===== type paramater variance");
    val invO = new Invariant[Object];
    val invS = new Invariant[String];
    Console.println( invO.isInstanceOf[Invariant[Object]]);
    Console.println(!invO.isInstanceOf[Invariant[String]]);
    Console.println(!invS.isInstanceOf[Invariant[Object]]);
    Console.println( invS.isInstanceOf[Invariant[String]]);
    val covO = new Covariant[Object];
    val covS = new Covariant[String];
    Console.println( covO.isInstanceOf[Covariant[Object]]);
    Console.println(!covO.isInstanceOf[Covariant[String]]);
    Console.println( covS.isInstanceOf[Covariant[Object]]);
    Console.println( covS.isInstanceOf[Covariant[String]]);
    val conO = new Contravariant[Object];
    val conS = new Contravariant[String];
    Console.println( conO.isInstanceOf[Contravariant[Object]]);
    Console.println( conO.isInstanceOf[Contravariant[String]]);
    Console.println(!conS.isInstanceOf[Contravariant[Object]]);
    Console.println( conS.isInstanceOf[Contravariant[String]]);
    val mulO = new MultiVariant[Object,Object,Object,Object,Object,Object];
    val mulS = new MultiVariant[String,String,String,String,String,String];
    Console.println( mulO.isInstanceOf[MultiVariant[Object,Object,Object,Object,Object,Object]]);
    Console.println( mulO.isInstanceOf[MultiVariant[Object,String,Object,Object,String,Object]]);
    Console.println( mulS.isInstanceOf[MultiVariant[String,String,String,String,String,String]]);
    Console.println(!mulS.isInstanceOf[MultiVariant[Object,Object,Object,Object,Object,Object]]);
    Console.println( mulS.isInstanceOf[MultiVariant[String,String,Object,String,String,Object]]);
  }
}

object TestSingletonTypes {
  def main(args: Array[String]): Unit = {
    Console.println("===== singleton types");
    val x: String = "x";
    val y: String = "y";
    Console.println( x.isInstanceOf[x.type]);
    Console.println(!x.isInstanceOf[y.type]);
  }
}

object TestCompoundTypes {
  class C, D;

  def main(args: Array[String]): Unit = {
    Console.println("===== compound types");
    val c = new C;
    val d = new D;
    Console.println(!c.isInstanceOf[C with D]);
    Console.println(!c.isInstanceOf[D with C]);
    Console.println(!d.isInstanceOf[C with D]);
    Console.println(!d.isInstanceOf[D with C]);

    val cd = new C with D;
    val dc = new D with C;
    Console.println(cd.isInstanceOf[C]);
    Console.println(cd.isInstanceOf[D]);
    Console.println(cd.isInstanceOf[C with D]);
    Console.println(cd.isInstanceOf[D with C]);
    Console.println(dc.isInstanceOf[C]);
    Console.println(dc.isInstanceOf[D]);
    Console.println(dc.isInstanceOf[C with D]);
    Console.println(dc.isInstanceOf[D with C]);
  }
}

object TestNull {
  class C;
  def main(args: Array[String]): Unit = {
    Console.println("===== null");
    Console.println(!null.isInstanceOf[Any]);
    Console.println(!null.isInstanceOf[AnyRef]);
    Console.println(!null.isInstanceOf[AllRef]);
    Console.println(!null.isInstanceOf[C]);
    Console.println((null.asInstanceOf[AnyRef]) == null);
    try {
      Console.println(null.asInstanceOf[Any]);
      Console.println("false (THIS SHOULD NOT BE PRINTED!!!)");
    } catch {
      case _: ClassCastException => ()
    }
  }
}

object TestDefaultValue {
  def defaultValue[T]: T = {
    class Cell[T] { var x: T = _; }
    return (new Cell[T]).x;
  }

  class C;

  def main(args: Array[String]): Unit = {
    Console.println("===== default value");
    Console.println(defaultValue[Boolean] == false);
    Console.println(defaultValue[Byte] == 0.asInstanceOf[Byte]);
    Console.println(defaultValue[Short] == 0.asInstanceOf[Short]);
    Console.println(defaultValue[Int] == 0);
    Console.println(defaultValue[Long] == 0.asInstanceOf[Long]);
    Console.println(defaultValue[Float] == 0.asInstanceOf[Float]);
    Console.println(defaultValue[Double] == 0.asInstanceOf[Double]);
    Console.println(defaultValue[String] == null);
    Console.println(defaultValue[C] == null);
  }
}

object TestArrays {
  def isInst[T](x: Any): Boolean = x.isInstanceOf[T];

  def main(args: Array[String]): Unit = {
    Console.println("===== arrays");

    import java.lang.reflect.{Array => JArray};

    val iArray = JArray.newInstance(Integer.TYPE, 5);
    Console.println(iArray.isInstanceOf[Array[Int]]);
    Console.println(isInst[Array[Int]](iArray));

    val oArray = JArray.newInstance(Class.forName("java.lang.Object"), 5);
    Console.println(oArray.isInstanceOf[Array[Object]]);
    Console.println(isInst[Array[Object]](oArray));
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    TestPolymorphicClasses.main(args);
    TestNestedClasses.main(args);
    TestClassesInsideFunctions.main(args);
    TestVariance.main(args);
    TestSingletonTypes.main(args);
    TestCompoundTypes.main(args);
    TestNull.main(args);
    TestDefaultValue.main(args);
    TestArrays.main(args);
  }
}
