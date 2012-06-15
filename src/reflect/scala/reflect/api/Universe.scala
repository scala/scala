package scala.reflect
package api

import language.experimental.macros

abstract class Universe extends base.Universe
                           with Symbols
                           with Types
                           with FlagSets
                           with Names
                           with Trees
                           with Printers
                           with Constants
                           with Positions
                           with Mirrors
                           with StandardDefinitions
                           with StandardNames
                           with Importers
                           with Exprs
                           with AnnotationInfos
{

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
   *      val $u: u.type = u // where u is a reference to the Universe that calls the reify
   *      $u.Expr[Int]($u.Apply($u.Select($u.Ident($u.newFreeVar("x", <Int>, x), "+"), List($u.Literal($u.Constant(1))))))
   *    ]>
   *  }}}
   *
   *  Reification performs expression splicing (when processing Expr.splice)
   *  and type splicing (for every type T that has a TypeTag[T] implicit in scope):
   *
   *  {{{
   *    val two = mirror.reify(2)                         // Literal(Constant(2))
   *    val four = mirror.reify(two.splice + two.splice)  // Apply(Select(two.tree, newTermName("$plus")), List(two.tree))
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
  // implementation is magically hardwired to `scala.reflect.reify.Taggers`
  def reify[T](expr: T): Expr[T] = macro ???
}