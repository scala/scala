package scala.reflect.makro

import language.experimental.macros

// todo. introduce context hierarchy
// the most lightweight context should just expose the stuff from the SIP
// the full context should include all traits from scala.reflect.makro (and probably reside in scala-compiler.jar)

trait Context extends Aliases
                 with CapturedVariables
                 with Enclosures
                 with Infrastructure
                 with Names
                 with Reifiers
                 with FrontEnds
                 with Settings
                 with Symbols
                 with Typers
                 with Util {

  /** The mirror that corresponds to the compile-time universe */
  val mirror: scala.reflect.api.Universe

  /** The type of the prefix tree from which the macro is selected */
  type PrefixType

  /** The prefix tree from which the macro is selected */
  val prefix: Expr[PrefixType]

  /** Alias to the underlying mirror's reify */
  def reify[T](expr: T): Expr[T] = macro Context.reify[T]
}

object Context {
  def reify[T](cc: Context{ type PrefixType = Context })(expr: cc.Expr[T]): cc.Expr[cc.prefix.value.Expr[T]] = {
    import cc.mirror._
    import scala.reflect.makro.internal._
    // [Eugene] how do I typecheck this without undergoing this tiresome (and, in general, incorrect) procedure?
    val prefix: Tree = Select(cc.prefix.tree, newTermName("mirror"))
    val prefixTpe = cc.typeCheck(TypeApply(Select(prefix, newTermName("asInstanceOf")), List(SingletonTypeTree(prefix)))).tpe
    prefix setType prefixTpe
    cc.Expr(cc.materializeExpr(prefix, expr.tree))
  }
}
