class A[U] {
  @annotation.varargs def m1[T]          (a: T*): T = a.head
  @annotation.varargs def m2[T <: AnyRef](a: T*): T = a.head
  @annotation.varargs def m3[T <: AnyVal](a: T*): T = a.head
  @annotation.varargs def m4[T <: Int]   (a: T*): T = a.head
  @annotation.varargs def m5[T <: String](a: T*): T = a.head
  @annotation.varargs def m6             (a: String*): String = a.head
  @annotation.varargs def m7             (a: Int*): Int = a.head
  @annotation.varargs def m8             (a: U*): U = a.head

  def n1[T]          (a: Array[T]): T = a(0)
  def n2[T <: AnyRef](a: Array[T]): T = a(0)
  def n3[T <: AnyVal](a: Array[T]): T = a(0)
  def n4[T <: Int]   (a: Array[T]): T = a(0)
  def n5[T <: String](a: Array[T]): T = a(0)
  def n6             (a: Array[String]): String = a(0)
  def n7             (a: Array[Int]): Int = a(0)
  def n8             (a: Array[U]): U = a(0)
}

object Test extends App {
  val a = classOf[A[_]]

  def sig   (method: String, tp: Class[_]) = a.getDeclaredMethod(method, tp).toString
  def genSig(method: String, tp: Class[_]) = a.getDeclaredMethod(method, tp).toGenericString
  def isVarArgs(method: String, tp: Class[_]) = a.getDeclaredMethod(method, tp).isVarArgs
  def bound (method: String, tp: Class[_]) = {
    val m = a.getDeclaredMethod(method, tp)
    m.getGenericParameterTypes.apply(0) match {
      case _: Class[_] => ""
      case gat: java.lang.reflect.GenericArrayType =>
        val compTp = gat.getGenericComponentType.asInstanceOf[java.lang.reflect.TypeVariable[_]]
        compTp.getBounds.apply(0).toString
    }
  }

  def check(found: String, expected: String): Unit =
    assert(found == expected, s"found: $found\nexpected: $expected")

  def checkVarArgs(method: String, tp: Class[_])(expected: String): Unit = {
    assert(isVarArgs(method, tp), s"expected varargs for $method")
    val found = genSig(method, tp)

    def varArgsToBraces(sig: String) = sig.replaceAll("""\.\.\.""","[]")
    // Normalize sigs so that the tests works on Java 6 (where varargs are printed as []) 
    // and above (where vargs are pretty printed using ...)
    assert(varArgsToBraces(found) == varArgsToBraces(expected), s"found: $found\nexpected: $expected (modulo `...` or `[]` as varargs suffix)")
  }

  val sq = classOf[Seq[_]]
  val ob = classOf[Object]
  val ao = classOf[Array[Object]]
  val as = classOf[Array[String]]
  val ai = classOf[Array[Int]]

  check(sig("m1", sq)   , "public java.lang.Object A.m1(scala.collection.Seq)")
  check(sig("m2", sq)   , "public java.lang.Object A.m2(scala.collection.Seq)")
  check(sig("m3", sq)   , "public java.lang.Object A.m3(scala.collection.Seq)")
  check(sig("m4", sq)   , "public int A.m4(scala.collection.Seq)")
  check(sig("m5", sq)   , "public java.lang.String A.m5(scala.collection.Seq)")
  check(sig("m6", sq)   , "public java.lang.String A.m6(scala.collection.Seq)")
  check(sig("m7", sq)   , "public int A.m7(scala.collection.Seq)")
  check(sig("m8", sq)   , "public java.lang.Object A.m8(scala.collection.Seq)")

  check(genSig("m1", sq), "public <T> T A.m1(scala.collection.Seq<T>)")
  check(genSig("m2", sq), "public <T> T A.m2(scala.collection.Seq<T>)")
  check(genSig("m3", sq), "public <T> T A.m3(scala.collection.Seq<T>)")
  // TODO: the signature for is wrong for T <: Int, SI-9846. The signature should be
  // `public int A.m4(scala.collection.Seq<java.lang.Object>)`. This is testing the status quo.
  check(genSig("m4", sq), "public <T> T A.m4(scala.collection.Seq<T>)")
  check(genSig("m5", sq), "public <T> T A.m5(scala.collection.Seq<T>)")
  check(genSig("m6", sq), "public java.lang.String A.m6(scala.collection.Seq<java.lang.String>)")
  check(genSig("m7", sq), "public int A.m7(scala.collection.Seq<java.lang.Object>)")
  check(genSig("m8", sq), "public U A.m8(scala.collection.Seq<U>)")


  // varargs forwarder

  check(sig("m1", ao)   , "public java.lang.Object A.m1(java.lang.Object[])")
  check(sig("m2", ao)   , "public java.lang.Object A.m2(java.lang.Object[])")
  check(sig("m3", ao)   , "public java.lang.Object A.m3(java.lang.Object[])")
  check(sig("m4", ao)   , "public int A.m4(java.lang.Object[])")
  check(sig("m5", as)   , "public java.lang.String A.m5(java.lang.String[])")
  check(sig("m6", as)   , "public java.lang.String A.m6(java.lang.String[])")
  check(sig("m7", ai)   , "public int A.m7(int[])")
  check(sig("m8", ao)   , "public java.lang.Object A.m8(java.lang.Object[])")

  checkVarArgs("m1", ao)("public <T> T A.m1(T...)")
  checkVarArgs("m2", ao)("public <T> T A.m2(T...)")
  checkVarArgs("m3", ao)("public <T> T A.m3(T...)")
  // testing status quo: signature is wrong for T <: Int, SI-9846
  checkVarArgs("m4", ao)("public <T> T A.m4(T...)")
  checkVarArgs("m5", as)("public <T> T A.m5(T...)")
  checkVarArgs("m6", as)("public java.lang.String A.m6(java.lang.String...)")
  checkVarArgs("m7", ai)("public int A.m7(int...)")
  checkVarArgs("m8", ao)("public U A.m8(U...)")

  check(bound("m1", ao) , "class java.lang.Object")
  check(bound("m2", ao) , "class java.lang.Object")
  check(bound("m3", ao) , "class java.lang.Object")
  check(bound("m4", ao) , "class java.lang.Object")
  check(bound("m5", as) , "class java.lang.String")
  check(bound("m6", as) , "")
  check(bound("m7", ai) , "")
  check(bound("m8", ao) , "class java.lang.Object")


  check(sig("n1", ob)   , "public java.lang.Object A.n1(java.lang.Object)")
  check(sig("n2", ao)   , "public java.lang.Object A.n2(java.lang.Object[])")
  check(sig("n3", ob)   , "public java.lang.Object A.n3(java.lang.Object)")
  check(sig("n4", ob)   , "public int A.n4(java.lang.Object)")
  check(sig("n5", as)   , "public java.lang.String A.n5(java.lang.String[])")
  check(sig("n6", as)   , "public java.lang.String A.n6(java.lang.String[])")
  check(sig("n7", ai)   , "public int A.n7(int[])")
  check(sig("n8", ob)   , "public java.lang.Object A.n8(java.lang.Object)")

  check(genSig("n1", ob), "public <T> T A.n1(java.lang.Object)")
  check(genSig("n2", ao), "public <T> T A.n2(T[])")
  check(genSig("n3", ob), "public <T> T A.n3(java.lang.Object)")
  // testing status quo: signature is wrong for T <: Int, SI-9846
  check(genSig("n4", ob), "public <T> T A.n4(java.lang.Object)")
  check(genSig("n5", as), "public <T> T A.n5(T[])")
  check(genSig("n6", as), "public java.lang.String A.n6(java.lang.String[])")
  check(genSig("n7", ai), "public int A.n7(int[])")
  check(genSig("n8", ob), "public U A.n8(java.lang.Object)")
}
