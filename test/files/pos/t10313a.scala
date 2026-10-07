//> using options -Werror -Xlint:unused
//-Vprint:typer -Vmacro

import scala.util.matching.Regex

// guard against regression in reporting constant `esc` unused
object Naming {
  private final val esc = "\u001b"
  private val csi = raw"$esc\[[0-9;]*([\x40-\x7E])"
  private lazy val cleaner = raw"$csi|([\p{Cntrl}&&[^\p{Space}]]+)|$linePattern".r
  private def linePattern: String = ""
  private def clean(m: Regex.Match): Option[String] = None
  def unmangle(str: String): String = cleaner.replaceSomeIn(str, clean)
}
