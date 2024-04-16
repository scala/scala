//> using options -opt:inline:**
class C {
  def t(a: A): AnyRef = {
    // a.a is inlined, resulting in a.b, which has return type B
    var foo: AnyRef = if (hashCode == 0) a.a else this
    if (foo == null)
      foo = this
    // at the merge point, the stack map frame calculation needs the LUB of (B, C),
    // so the ClassBType for C needs to be cached
    foo
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    new C
  }
}
