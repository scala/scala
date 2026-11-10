//> using options -Ystop-after:pickler

// superaccessors consults erasure's phase travel for `super[J].m` on a Java default method,
// which used to throw an NPE when erasure is not part of the run.
class C extends J {
  override def greet(w: String): String = super[J].greet(w) + "!"
  def viaClosure(ws: List[String]): List[String] = ws.map(w => super[J].greet(w))
}
