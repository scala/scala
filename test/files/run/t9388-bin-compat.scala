class C {
  private object N extends Serializable { override def toString = "N" }
  def foo = N.toString
}
object Test {
  def main(args: Array[String]): Unit = {
    val c = Class.forName("C")
    assert(c.getDeclaredFields().toList.map(_.toString) ==
      List("private volatile C$N$ C.C$$N$module")) // field is name-mangled (C$$N$module instead of just N$module)
    assert(c.getDeclaredMethods().toList.map(_.toString).sorted ==
      List("private C$N$ C.C$$N$lzycompute()",
           "public C$N$ C.C$$N()",
           "public java.lang.String C.foo()")) // accessor is public, name-mangled
    assert((new C).foo == "N")
  }
}
