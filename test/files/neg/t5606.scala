//> using options -Xsource:3
// was: _ taken as ident of type param, but poor interactions below
case class CaseTest[_](someData: String)

case class CaseTest_?[?](someData: String)

// was: _ already defined
case class CaseTest2[_, _](someData: String)

class C {
  def f[_](x: Int) = ???
}

object Test extends App {
  def f0 = new CaseTest("X")
  def f1: CaseTest[Int] = new CaseTest[Int]("X")  // OK!
  def f2: CaseTest[Int] = CaseTest[Int]("X")      // CaseTest[Any]
  def f3 = new CaseTest[Int]("X").copy()          // CaseTest[Any]
  def f4 = new CaseTest[Int]("X").copy[Int]()     // CaseTest[Any]

  def regress0[F[_]]    = 0
  def regress1[F[_, _]] = 1
  def regress_?[F[?]]   = 2
  //def regress0[F[_$$1]] = 0;
  //def regress1[F[_$$2, _$$3]] = 1
}
