package scala
package reflect

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  The base package for Scala macros.
 *
 *  Macros are functions that are called by the compiler during compilation.
 *  Within these functions the programmer has access to compiler APIs.
 *  For example, it is possible to generate, analyze and typecheck code.
 *
 *  See the [[http://docs.scala-lang.org/overviews/macros/overview.html Macros Guide]] on how to get started with Scala macros.
 */
package object macros {
  /** The Scala macros context.
   *
   *  In Scala 2.11, macros that were once the one are split into blackbox and whitebox macros,
   *  with the former being better supported and the latter being more powerful. You can read about
   *  the details of the split and the associated trade-offs in the [[http://docs.scala-lang.org/overviews/macros/overview.html Macros Guide]].
   *
   *  `scala.reflect.macros.Context` follows this tendency and turns into `scala.reflect.macros.blackbox.Context`
   *  and `scala.reflect.macros.whitebox.Context`. The original `Context` is left in place for compatibility reasons,
   *  but it is now deprecated, nudging the users to choose between blackbox and whitebox macros.
   */
  @deprecated("use blackbox.Context or whitebox.Context instead", "2.11.0")
  type Context = whitebox.Context
}
