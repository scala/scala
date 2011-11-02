class Test {
  def test {
    val d = new Defs
    val u = d.i + 1
    d.i = 2
    val v = d.bar()
    val i = new d.Inner
    val w = i.buz()
  }
  
  @deprecated("no longer!", "") class Inner {
    @deprecated("uncool", "") def f: Int = 1
    @deprecated("this one as well!", "") var g = -1
  }
}

object Test { def main(args: Array[String]) { } }
