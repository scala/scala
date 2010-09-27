case class DebugParam[T](param: T)

// TypeStack init: REFERENCE(type AnyRef)
// [Now checking: typer]
// [check: typer] work/fail1.scala:1: trees differ
//  old: T [Trees$Ident] (tpe = T)
//  new: T [Trees$TypeTree] (tpe = T)
// [check: typer] work/fail1.scala:1: trees differ
//  old: DebugParam[T] [Trees$AppliedTypeTree] (tpe = null)
//  new: DebugParam[T] [Trees$TypeTree] (tpe = DebugParam[T])
// Exception in thread "main" java.lang.NullPointerException
//  at scala.tools.nsc.typechecker.Typers$Typer.typedTypeConstructor(Typers.scala:4337)
//  at scala.tools.nsc.typechecker.Typers$Typer.typedTypeConstructor(Typers.scala:4358)
//  at scala.tools.nsc.typechecker.Typers$Typer.typedNew$1(Typers.scala:3240)
//  at scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:3994)
//  at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:4223)
//  at scala.tools.nsc.typechecker.TreeCheckers$TreeChecker.scala$tools$nsc$typechecker$TreeCheckers$TreeChecker$$super$typed(TreeCheckers.scala:101)
