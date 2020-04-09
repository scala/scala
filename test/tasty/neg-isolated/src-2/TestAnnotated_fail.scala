package tastytest

object TestAnnotated {

  def test1 = new Annotated {} // error: can't find type required by a member of package tastytest: tastytest.annot
  def test2 = new PublicAnnotated {} // error: could not find class tastytest.Parent
  def test3 = new SymbollicAnnotated {} // error: could not find class tastytest.<<<
  def test4 = new PublicSymbollicAnnotated {} // ok
  def test5 = new PublicPkgAnnotated {} // ok

}
