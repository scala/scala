final class Opt[+A >: Null](val value: A) extends AnyVal {
  def get: A  = value
  def isEmpty = value == null
}
object Opt {
  final val None = new Opt[Null](null)
  def apply[A >: Null](value: A): Opt[A] = if (value == null) None else new Opt[A](value)
}

object ValueOpt {
  // public java.lang.String unapply(java.lang.Object);
  //        0: aload_1
  //        1: instanceof    #16                 // class java/lang/String
  //        4: ifeq          21
  //        7: getstatic     #21                 // Field Opt$.MODULE$:LOpt$;
  //       10: astore_2
  //       11: ldc           #23                 // String String
  //       13: checkcast     #16                 // class java/lang/String
  //       16: astore        5
  //       18: goto          71
  //       21: aload_1
  //       22: instanceof    #25                 // class scala/collection/immutable/List
  //       25: ifeq          42
  //       28: getstatic     #21                 // Field Opt$.MODULE$:LOpt$;
  //       31: astore_3
  //       32: ldc           #27                 // String List
  //       34: checkcast     #16                 // class java/lang/String
  //       37: astore        5
  //       39: goto          71
  //       42: aload_1
  //       43: instanceof    #29                 // class java/lang/Integer
  //       46: ifeq          64
  //       49: getstatic     #21                 // Field Opt$.MODULE$:LOpt$;
  //       52: astore        4
  //       54: ldc           #31                 // String Int
  //       56: checkcast     #16                 // class java/lang/String
  //       59: astore        5
  //       61: goto          71
  //       64: getstatic     #21                 // Field Opt$.MODULE$:LOpt$;
  //       67: pop
  //       68: aconst_null
  //       69: astore        5
  //       71: aload         5
  //       73: areturn
  def unapply(x: Any): Opt[String] = x match {
    case _: String  => Opt("String")
    case _: List[_] => Opt("List")
    case _: Int     => Opt("Int")
    case _          => Opt.None
  }
}
object RegularOpt {
  // public scala.Option<java.lang.String> unapply(java.lang.Object);
  //        0: aload_1
  //        1: instanceof    #16                 // class java/lang/String
  //        4: ifeq          20
  //        7: new           #18                 // class scala/Some
  //       10: dup
  //       11: ldc           #20                 // String String
  //       13: invokespecial #23                 // Method scala/Some."<init>":(Ljava/lang/Object;)V
  //       16: astore_2
  //       17: goto          64
  //       20: aload_1
  //       21: instanceof    #25                 // class scala/collection/immutable/List
  //       24: ifeq          40
  //       27: new           #18                 // class scala/Some
  //       30: dup
  //       31: ldc           #27                 // String List
  //       33: invokespecial #23                 // Method scala/Some."<init>":(Ljava/lang/Object;)V
  //       36: astore_2
  //       37: goto          64
  //       40: aload_1
  //       41: instanceof    #29                 // class java/lang/Integer
  //       44: ifeq          60
  //       47: new           #18                 // class scala/Some
  //       50: dup
  //       51: ldc           #31                 // String Int
  //       53: invokespecial #23                 // Method scala/Some."<init>":(Ljava/lang/Object;)V
  //       56: astore_2
  //       57: goto          64
  //       60: getstatic     #36                 // Field scala/None$.MODULE$:Lscala/None$;
  //       63: astore_2
  //       64: aload_2
  //       65: areturn
  def unapply(x: Any): Option[String] = x match {
    case _: String  => Some("String")
    case _: List[_] => Some("List")
    case _: Int     => Some("Int")
    case _          => None
  }
}

object Test {
  def f(x: Any) = x match {
    case ValueOpt(s) => s
    case _           => "Something else"
  }
  def g(x: Any) = x match {
    case RegularOpt(s) => s
    case _             => "Something else"
  }
  val xs = List("abc", Nil, 5, Test)

  def main(args: Array[String]): Unit = {
    xs map f foreach println
    xs map g foreach println
  }
}
