object Test {
  def pf(pf: PartialFunction[Any, Unit]) = ()
  def f1(pf: Function[Any, Unit]) = ()
 
  class A1; class B1
  def test1(x: String, x1: String, default: String) = pf {
    case _ if (
         x.isEmpty
      && default.isEmpty // was binding to synthetic param
      && x1.isEmpty      // was binding to synthetic param
    ) =>
      x.isEmpty
      default.isEmpty // was binding to synthetic param
      x1.isEmpty      // was binding to synthetic param
      new A1; new B1
  }

  def test2(x: String, x1: String, default: String) = f1 {
    case _ if (
         x.isEmpty
      && default.isEmpty
      && x1.isEmpty
    ) =>
      x.isEmpty
      default.isEmpty
      x1.isEmpty
      new A1; new B1
  }
}
