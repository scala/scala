package scala.reflect.makro
package runtime

trait Symbols {
  self: Context =>

  def isLocatable(sym: Symbol) = sym.isLocatable

  def isStatic(sym: Symbol) = sym.isStatic
}