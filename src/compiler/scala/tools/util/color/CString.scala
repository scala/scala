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
  def bytes()         = colorized map (_.toByte)
  def >()             = show()
  
  def append(x: CString): CString = new CString(uncolorized + x.uncolorized, colorized + x.colorized)
  def +(other: CString): CString  = this append other

  override def toString = colorized
}

class CStringOps(str: String) {
  /** String to String operation.
   *    println("foo" in Red)
   *    println("bar" in Magenta.bright)
   */
  def in(ansi: Ansi): String = ansi colorize str

  /** Gave in to one bit of punctuation, because everyone adds
   *  strings with '+' and we need something which higher precedence
   *  for it to be at all satisfying.
   *
   *  "foo" %> Red + "bar" %> Magenta.bright
   */
  def %>(ansi: Ansi): CString = new CString(str, in(ansi))
}
