//> using options -Xlint -Werror
//
// was: _ taken as ident of type param, now a fresh name
case class CaseTest[_](someData: String)

// was: _ already defined, now a fresh name
case class CaseTest2[_, _](someData: String)

class C {
  def f[_](x: Int) = ???
}
