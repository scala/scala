package foo

trait ArgumentExprs1 {
  def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) = 1
  f(
    23,
    "bar",
  )(
    Ev0,
    Ev1,
  )

  // test arg exprs in the presence of varargs
  def g(x: Int, y: Int*) = 1
  g(1,2,
  )
  g(1,List(2, 3): _*,
  )
}

trait ArgumentExprs2 {
  class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1)
  new C(
    23,
    "bar",
  )(
    Ev0,
    Ev1,
  )
}

trait Params {
  def f(
    foo: Int,
    bar: String,
  )(implicit
    ev0: Ev0,
    ev1: Ev1,
  )
}

trait ClassParams {
  class C(
    foo: Int,
    bar: String,
  )(implicit
    ev0: Ev0,
    ev1: Ev1,
  )

  // test class params in the precense of varargs
  case class D(i: Int*,
  )
}

trait SimpleExpr1 {
  def f: (Int, String) = (
    23,
    "bar",
  )

  // the Tuple1 value case, the trailing comma is ignored so the type is Int and the value 23
  def g: Int = (
    23,
  )
}

trait TypeArgs {
  class C[A, B]
  def f: C[
    Int,
    String,
  ]
}

trait TypeParamClause {
  class C[
    A,
    B,
  ]
}

trait FunTypeParamClause {
  def f[
    A,
    B,
  ]
}

trait SimpleType {
  def f: (
    Int,
    String,
  )

  // the Tuple1 type case, the trailing comma is ignored so the type is Int and the value 23
  def g: (
    Int,
  ) = 23
}

trait FunctionArgTypes {
  def f: (
    Int,
    String,
  ) => Boolean
}

trait SimplePattern {
  val (
    foo,
    bar,
  ) = null: Any

  // test '@' syntax in patterns
  Some(1) match {
    case Some(x @ 1,
    ) => x
  }

  // test ': _*' syntax in patterns
  List(1, 2, 3) match {
    case List(1, 2, _ @ _*,
    ) => 1
  }

  // test varargs in patterns
  val List(x, y, _*,
  ) = 42 :: 17 :: Nil
}

trait ImportSelectors {
  import foo.{
    Ev0,
    Ev1,
  }
}

trait Bindings {
  def g(f: (Int, String) => Boolean)

  g((
    foo,
    bar,
  ) => true)
}

// Import, ids, ValDcl, VarDcl, VarDef, PatDef use commas, but not inside paren, bracket or brace,
// so they don't support an optional trailing comma

// test utilities
object `package` {
  sealed trait Ev0; implicit object Ev0 extends Ev0
  sealed trait Ev1; implicit object Ev1 extends Ev1
}
