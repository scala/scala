package scala.reflect
package api

import language.experimental.macros

abstract class Universe extends Symbols
                           with FreeVars
                           with Types
                           with Constants
                           with Scopes
                           with Names
                           with Trees
                           with AnnotationInfos
                           with Positions
                           with Exprs
                           with StandardDefinitions
                           with TypeTags
                           with TreePrinters
                           with StandardNames
                           with ClassLoaders
                           with TreeBuildUtil
                           with ToolBoxes
                           with FrontEnds
                           with Importers {

  /** Given an expression, generate a tree that when compiled and executed produces the original tree.
   *  The produced tree will be bound to the Universe it was called from.
   *
   *  For instance, given the abstract syntax tree representation of the <[ x + 1 ]> expression:
   *
   *  {{{
   *    Apply(Select(Ident("x"), "+"), List(Literal(Constant(1))))
   *  }}}
   *
   *  The reifier transforms it to the following expression:
   *   
   *  {{{
   *    <[
   *      val $mr: scala.reflect.api.Universe = <reference to the Universe that calls the reify>
   *      $mr.Expr[Int]($mr.Apply($mr.Select($mr.Ident($mr.newFreeVar("x", <Int>, x), "+"), List($mr.Literal($mr.Constant(1))))))
   *    ]>
   *  }}}
   *
   *  Reification performs expression splicing (when processing Expr.eval and Expr.value)
   *  and type splicing (for every type T that has a TypeTag[T] implicit in scope):
   *
   *  {{{
   *    val two = mirror.reify(2)                       // Literal(Constant(2))
   *    val four = mirror.reify(two.eval + two.eval)    // Apply(Select(two.tree, newTermName("$plus")), List(two.tree))
   *
   *    def macroImpl[T](c: Context) = {
   *      ...
   *      // T here is just a type parameter, so the tree produced by reify won't be of much use in a macro expansion
   *      // however, if T were annotated with c.TypeTag (which would declare an implicit parameter for macroImpl)
   *      // then reification would subtitute T with the TypeTree that was used in a TypeApply of this particular macro invocation
   *      val factory = c.reify{ new Queryable[T] }
   *      ...
   *    }
   *  }}}
   *
   *  The transformation looks mostly straightforward, but it has its tricky parts:
   *    * Reifier retains symbols and types defined outside the reified tree, however
   *      locally defined entities get erased and replaced with their original trees
   *    * Free variables are detected and wrapped in symbols of the type FreeVar
   *    * Mutable variables that are accessed from a local function are wrapped in refs
   *    * Since reified trees can be compiled outside of the scope they've been created in,
   *      special measures are taken to ensure that all members accessed in the reifee remain visible
   */
  def reify[T](expr: T): Expr[T] = macro Universe.reify[T]
}

object Universe {
  def reify[T](cc: scala.reflect.makro.Context{ type PrefixType = Universe })(expr: cc.Expr[T]): cc.Expr[cc.prefix.value.Expr[T]] = {
    import scala.reflect.makro.internal._
    cc.Expr(cc.materializeExpr(cc.prefix.tree, expr.tree))
  }
}
