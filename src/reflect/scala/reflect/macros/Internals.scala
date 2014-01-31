package scala
package reflect
package macros

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *  @see [[scala.reflect.api.Internals]]
 */
trait Internals {
  self: blackbox.Context =>

  /** @see [[scala.reflect.api.Internals]] */
  val internal: ContextInternalApi

  /** @see [[scala.reflect.api.Internals]] */
  trait ContextInternalApi extends universe.MacroInternalApi {
  }
}
