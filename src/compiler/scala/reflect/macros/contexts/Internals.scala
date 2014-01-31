package scala.reflect.macros
package contexts

trait Internals {
  self: Context =>

  lazy val internal: ContextInternalApi = new global.SymbolTableInternal with ContextInternalApi {
  }
}