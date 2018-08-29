
// scalac: -Ywarn-value-discard -Xfatal-warnings

// Ur-assignment operator := looks like omitted Unit syntax,
// which replaces procedure syntax.
//
// This is "safe" because the extra colon signals
// that inferred Unit is required.
//
// That also means that "value discard" is ignored when warning is enabled.

import language.experimental.macros

trait T {
  // omitted Unit
  def f(): = println("hello, brave new world")

  // omitted Unit with no space, using Ur-assignment operator
  def g() := println("hello, brave new world")

  // error on warning, Unit is explicit
  def h() := { println("I don't care (I love it)") ; 42 }

  // error on warning, Unit is explicit, old style
  def fail(): Unit = 42

  // ordinary inferred result type
  def whatever() = 42

  // should still be happy
  def m() := macro M.m
}

object M {
  import scala.reflect.macros.blackbox.Context
  def m(c: Context)() = {
    import c.universe._
    Literal(Constant(()))
  }
}
