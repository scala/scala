package tastytest

import CtorUsingClauses._
import CtorUsingClause._

object TestCtorUsingClauses extends Suite("TestCtorUsingClauses") {
  // TODO: test unpickle calling a ctor with a using clause
  
  test(assert(new CtorUsingClause(implicitly[Int])("hello").i === 23))
  test(assert(new Contextual(implicitly[Int])().i === 23))

  test(assert(new CtorUsingClauses.Sub1().i === 23))
  test(assert(new CtorUsingClauses.Sub2().i === 23))
  test(assert(new CtorUsingClauses.Sub3().i === 23))

  test(assert(new CtorUsingClauses.Annotated().j === 47))
  test(assert(new CtorUsingClauses.AnnotatedOld().k === 97))
}
