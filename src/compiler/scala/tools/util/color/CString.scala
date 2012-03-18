package scala.tools.util
package color

/** A colorized String.  It's difficult to achieve precise
 *  formatting and selective string colorization simultaneously,
 *  because all length-based calculations will break down in
 *  the face of the ansi controls.  It doesn't do much yet, but
 *  this is here to eventually make that transparent.
 */
final class CString(val uncolorized: String, val colorized: String) {
  def visibleLength   = uncolorized.length
  def colorizedLength = colorized.length
  def show()          = Console println colorized
  def bytes()         = colorized map (ch => ch.toByte)
  def >               = show()
  
  def append(x: CString): CString = new CString(uncolorized + x.uncolorized, colorized + x.colorized)
  def +(other: CString): CString  = this append other
  override def toString = colorized
}

class CStringOps(str: String) {
  /** Enables for example
   *    println("foo" in Red)
   *    println("foo" in Magenta.bright)
   */
  def in(ansi: Ansi): CString = new CString(str, ansi colorize str)
}
