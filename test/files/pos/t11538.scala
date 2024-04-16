//> using options -Werror -deprecation -stop:refchecks
package t11538

@deprecated("not for you", since = "just now")
class Abhorrent

object Bizzle {
  @deprecated("use mipple instead", since = "recently")
  val wibble: Abhorrent = mipple
  @deprecated("use wobble instead", since = "recently")
  def mipple: Abhorrent = wobble
  @deprecated("use wibble instead", since = "recently")
  var wobble: Abhorrent = wibble
}
