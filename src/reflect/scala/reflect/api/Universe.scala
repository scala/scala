package scala.reflect
package api

abstract class Universe extends Symbols
                           with Types
                           with FlagSets
                           with Scopes
                           with Names
                           with Trees
                           with Constants
                           with Annotations
                           with Positions
                           with Exprs
                           with TypeTags
                           with TagInterop
                           with StandardDefinitions
                           with StandardNames
                           with BuildUtils
                           with Mirrors
                           with Printers
                           with Importers
{
  /** Produce the abstract syntax tree representing the given Scala expression.
   *
   * For example
   *
   * {{{
   * val five = reify{ 5 }    // Literal(Constant(5))
   * reify{ 2 + 4 }           // Apply( Select( Literal(Constant(2)), newTermName("$plus")), List( Literal(Constant(4)) ) )
   * reify{ five.splice + 4 } // Apply( Select( Literal(Constant(5)), newTermName("$plus")), List( Literal(Constant(4)) ) )
   * }}}
   *
   * The produced tree is path dependent on the Universe `reify` was called from.
   *
   * Use [[scala.reflect.api.Exprs#Expr.splice]] to embed an existing expression into a reify call. Use [[Expr]] to turn a [[Tree]] into an expression that can be spliced.
   *
   * == Further info and implementation details ==
   *
   * `reify` is implemented as a macro, which given an expression, generates a tree that when compiled and executed produces the original tree.
   *
   *  For instance in `reify{ x + 1 }` the macro `reify` receives the abstract syntax tree of `x + 1` as its argument, which is
   *
   *  {{{
   *    Apply(Select(Ident("x"), "+"), List(Literal(Constant(1))))
   *  }}}
   *
   *  and returns a tree, which produces the tree above, when compiled and executed. So in other terms, the refiy call expands to something like
   *
   *  {{{
   *      val $u: u.type = u // where u is a reference to the Universe that calls the reify
   *      $u.Expr[Int]($u.Apply($u.Select($u.Ident($u.newFreeVar("x", <Int>, x), "+"), List($u.Literal($u.Constant(1))))))
   *  }}}
   *
   *  ------
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
   *      // however, if T were annotated with c.WeakTypeTag (which would declare an implicit parameter for macroImpl)
   *      // then reification would substitute T with the TypeTree that was used in a TypeApply of this particular macro invocation
   *      val factory = c.reify{ new Queryable[T] }
   *      ...
   *    }
   *  }}}
   *
   *  The transformation looks mostly straightforward, but it has its tricky parts:
   *    - Reifier retains symbols and types defined outside the reified tree, however
   *      locally defined entities get erased and replaced with their original trees
   *    - Free variables are detected and wrapped in symbols of the type `FreeTermSymbol` or `FreeTypeSymbol`
   *    - Mutable variables that are accessed from a local function are wrapped in refs
   */
  // implementation is hardwired to `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  def reify[T](expr: T): Expr[T] = ??? // macro
}