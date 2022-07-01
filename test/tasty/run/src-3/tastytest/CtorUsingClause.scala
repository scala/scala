package tastytest

import CtorUsingClauses.CtorUsingClause.given

object CtorUsingClauses {
  class CtorUsingClause(using x: Int)(y: String):
    val i = x
  
  object CtorUsingClause {
    given Int = 23
  }

  class Contextual(using x: Int): // ctor: <init>(using x: Int)()
    val i = x

  class Implicit(implicit x: Int): // ctor: <init>()(implicit x: Int)
    val i = x

  class Sub1()(using x: Int) extends CtorUsingClause("Sub")
  class Sub2()(using x: Int) extends Contextual
  class Sub3()(using x: Int) extends Implicit

  class CtxAnnot(using x: Int) extends scala.annotation.Annotation // ctor: <init>(using x: Int)()
  class CtxAnnotOld(implicit x: Int) extends scala.annotation.Annotation // ctor: <init>()(implicit x: Int)

  @CtxAnnot
  class Annotated:
    val j = 47

  @CtxAnnotOld
  class AnnotatedOld:
    val k = 97
}
