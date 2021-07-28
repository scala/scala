package tastytest

import issue12420._
import issue12420.{ShareLambda => sl}

object TestIssue12420 extends Suite("TestIssue12420") {

  def foo = new Foo
  def eta = new Eta

  test(assert(foo.bar.id.id == "Foo"))

  test(foo.bar match { case User(UserId(id: String)) => assert(id == "Foo") })

  test(assert(eta.inner == Boxxy.default))

  test(assert(new sl.Foo[sl.Bar].foo(new sl.Bar[List]) == "Bar"))

}
