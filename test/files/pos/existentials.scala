import language.existentials

/** All of these should work, some don't yet.
 *  !!!
 */
class A {
  def f() = { case class Bob(); Bob }

  val quux0 = f()
  def quux1 = f()
  lazy val quux2 = f()
  def quux3 = {
    lazy val quux3a = f()
    quux3a
  }

  // spurious warning until scala/bug#10612, cf test/files/neg/t7187.scala
  val bippy0 = f _
  def bippy1 = f _
  lazy val bippy2 = f _
  /*
  val bippy3 = {
    lazy val bippy3a = f _
    bippy3a
  }
  */
}

/*
error: scala.reflect.internal.Types$TypeError: type mismatch;
 found   : () => Bob.type(in value $anonfun) forSome { type Bob.type(in value $anonfun) <: scala.runtime.AbstractFunction0[Bob(in value $anonfun)] with Serializable{case def unapply(x$0: Bob(in value $anonfun)): Boolean} with Singleton; type Bob(in value $anonfun) <: Product with Serializable{def copy(): Bob(in value $anonfun)} }
 required: () => (some other)Bob.type(in value $anonfun) forSome { type (some other)Bob.type(in value $anonfun) <: scala.runtime.AbstractFunction0[(some other)Bob(in value $anonfun)] with Serializable{case def unapply(x$0: (some other)Bob(in value $anonfun)): Boolean} with Singleton; type (some other)Bob(in value $anonfun) <: Product with Serializable{def copy(): (some other)Bob(in value $anonfun)} }

  at scala.tools.nsc.typechecker.Contexts$ThrowingReporter.handleError(Contexts.scala:1426)
  at scala.tools.nsc.typechecker.Contexts$ContextReporter.issue(Contexts.scala:1278)
  at scala.tools.nsc.typechecker.Contexts$Context.issue(Contexts.scala:584)
  at scala.tools.nsc.typechecker.ContextErrors$ErrorUtils$.issueTypeError(ContextErrors.scala:106)
  at scala.tools.nsc.typechecker.ContextErrors$ErrorUtils$.issueNormalTypeError(ContextErrors.scala:99)
  at scala.tools.nsc.typechecker.ContextErrors$TyperContextErrors$TyperErrorGen$.AdaptTypeError(ContextErrors.scala:219)
  at scala.tools.nsc.typechecker.Typers$Typer.adaptMismatchedSkolems$1(Typers.scala:1058)
  at scala.tools.nsc.typechecker.Typers$Typer.lastTry$1(Typers.scala:1069)
  at scala.tools.nsc.typechecker.Typers$Typer.adaptExprNotFunMode$1(Typers.scala:1124)
  at scala.tools.nsc.typechecker.Typers$Typer.vanillaAdapt$1(Typers.scala:1170)
  at scala.tools.nsc.typechecker.Typers$Typer.adapt(Typers.scala:1214)
  at scala.tools.nsc.typechecker.Typers$Typer.runTyper$1(Typers.scala:5598)
  at scala.tools.nsc.typechecker.Typers$Typer.typedInternal(Typers.scala:5616)
  at scala.tools.nsc.typechecker.Typers$Typer.body$2(Typers.scala:5557)
  at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:5562)
  at scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typedArg$1(Typers.scala:3247)
  at scala.tools.nsc.typechecker.Typers$Typer.typedArg(Typers.scala:477)
*/
