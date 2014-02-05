object Test {
  def bar(s: String) = s;
  val o: Option[Null] = None
  def nullReference {
    val a: Null = o.get
    bar(a) // Was: VerifyError under GenICode
  }

  def literal {
    val a: Null = null
    bar(a)
  }

  def main(args: Array[String]) = {
    try { nullReference } catch { case _: NoSuchElementException => }
    literal
  }
}
