/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.transform.async
package user

import scala.reflect.api.Universe
import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.macros.Aliases
import scala.reflect.macros.whitebox.Context

/**
 * A base class for the `async` macro. Subclasses must provide:
 *
 * - Concrete types for a given future system
 * - Tree manipulations to create and complete the equivalent of Future and Promise
 * in that system.
 * - The `async` macro declaration itself, and a forwarder for the macro implementation.
 * (The latter is temporarily needed to workaround bug SI-6650 in the macro system)
 *
 * The default implementation, [[scala.async]], binds the macro to `scala.concurrent._`.
 */
abstract class AsyncBase {
  type FS <: FutureSystem
  val futureSystem: FS

//  /**
//   * A call to `await` must be nested in an enclosing `async` block.
//   *
//   * A call to `await` does not block the current thread, rather it is a delimiter
//   * used by the enclosing `async` macro. Code following the `await`
//   * call is executed asynchronously, when the argument of `await` has been completed.
//   *
//   * @param awaitable the future from which a value is awaited.
//   * @tparam T        the type of that value.
//   * @return          the value.
//   */
//  @compileTimeOnly("`await` must be enclosed in an `async` block")
//  def await[T](awaitable: futureSystem.Fut[T]): T = ???
//
//  def asyncImpl[T: c.WeakTypeTag](c: Context)
//                                 (body: c.Expr[T])
//                                 (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = {
//
//    // I think there's a bug in subtyping -- shouldn't be necessary to specify the Aliases parent explicitly
//    // (but somehow we don't rebind the abstract types otherwise)
//    val ctx = c.asInstanceOf[Aliases with scala.reflect.macros.whitebox.Context {val universe: asyncMacro.u.type}]
//    val asyncMacroSymbol = ctx.macroApplication.symbol
//
//    object asyncMacro extends AsyncTransform(AsyncBase.this, c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]) {
//      // The actual transformation happens in terms of the internal compiler data structures.
//      // We hide the macro context from the classes in the transform package,
//      // because they're basically a compiler plugin packaged as a macro.
//      import u._
//
//      // TODO AM: rework
//      val asyncNames: AsyncNames[u.type] = rootMirror.RootClass.attachments.get[AsyncNames[u.type]].getOrElse {
//        val names = new AsyncNames[u.type](u)
//        rootMirror.RootClass.attachments.update(names)
//        names
//      }
//
//      val Async_async = asyncMethod(u)(asyncMacroSymbol)
//      val Async_await = awaitMethod(u)(asyncMacroSymbol)
//
//      // a few forwarders to context, since they are not easily available through SymbolTable
//      def typecheck(tree: Tree): Tree = ctx.typecheck(tree)
//      def abort(pos: Position, msg: String): Nothing = ctx.abort(pos, msg)
//      def error(pos: Position, msg: String): Unit = ctx.error(pos, msg)
//      val typingTransformers = new TypingTransformers {
//        val callsiteTyper: global.analyzer.Typer = ctx.asInstanceOf[scala.reflect.macros.contexts.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
//      }
//    }
//
//    val enclosingOwner = ctx.internal.enclosingOwner
//    val code =
//      try asyncMacro.asyncTransform(body.tree.asInstanceOf[ctx.Tree], execContext.tree.asInstanceOf[ctx.Tree], enclosingOwner, asyncMacroSymbol.pos.makeTransparent)(ctx.weakTypeTag[T].tpe)
//      catch { case te: ctx.universe.TypeError => ctx.info(enclosingOwner.pos, te.getStackTrace.mkString("\n"), true); ??? }
//
//    AsyncUtils.vprintln(s"async state machine transform expands to:\n $code")
//
//    // Mark range positions for synthetic code as transparent to allow some wiggle room for overlapping ranges
//    for (t <- code) t.setPos(t.pos.makeTransparent)
//    c.Expr[futureSystem.Fut[T]](code.asInstanceOf[c.Tree])
//  }
//
//
//  protected[async] def asyncMethod(u: Universe)(asyncMacroSymbol: u.Symbol): u.Symbol = {
//    import u._
//    if (asyncMacroSymbol == null) NoSymbol
//    else asyncMacroSymbol.owner.typeSignature.member(TermName("async"))
//  }
//
//  protected[async] def awaitMethod(u: Universe)(asyncMacroSymbol: u.Symbol): u.Symbol = {
//    import u._
//    if (asyncMacroSymbol == null) NoSymbol
//    else asyncMacroSymbol.owner.typeSignature.member(TermName("await"))
//  }

  protected[async] def nullOut(u: Universe)(name: u.Expr[String], v: u.Expr[Any]): u.Expr[Unit] =
    u.reify { () }
}
