package foo

// Note: Using traits to get distinct errors
// (instead of sharing one single "')' expected but '}' found." at the end)



//// Multi-line only cases: make sure trailing commas are only supported when multi-line

trait ArgumentExprs1 { f(23, "bar", )(Ev0, Ev1) }
trait ArgumentExprs2 { f(23, "bar")(Ev0, Ev1, ) }
trait ArgumentExprs3 { new C(23, "bar", )(Ev0, Ev1) }
trait ArgumentExprs4 { new C(23, "bar")(Ev0, Ev1, ) }

trait Params1 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1 }
trait Params2 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1 }
trait ClassParams1 { final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1) }
trait ClassParams2 { final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, ) }

trait SimpleExpr { (23, "bar", ) }

trait TypeArgs { def f: C[Int, String, ] }
trait TypeParamClause { type C[A, B, ] }
trait FunTypeParamClause { def f[A, B, ] }

trait SimpleType { def f: (Int, String, ) }
trait FunctionArgTypes { def f: (Int, String, ) => Boolean }

trait SimplePattern { val (foo, bar, ) = null: Any }

trait ImportSelectors { import foo.{ Ev0, Ev1, } }

trait Import { import foo.Ev0, foo.Ev1, }

trait ValDcl { val foo, bar, = 23 }
trait VarDcl { var foo, bar, = 23 }
trait VarDef { var foo, bar, = _ }
trait PatDef { val Foo(foo), Bar(bar), = bippy }



//// The Tuple 1 cases

// the Tuple1 value case: make sure that the possible "(23, )" syntax for Tuple1 doesn't compile to "23"
trait SimpleExpr2 { (23, ) }

// the Tuple1 type case: make sure that the possible "(Int, )" syntax for Tuple1[Int] doesn't compile to "Int"
trait SimpleType2 { def f: (Int, ) }



//// Test utilities
object `package` {
  sealed trait Ev0; implicit object Ev0 extends Ev0
  sealed trait Ev1; implicit object Ev1 extends Ev1
}
