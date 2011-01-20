/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml

/** An XML node for unparsed content. It will be output verbatim, all bets
 *  are off regarding wellformedness etc.
 *
 * @author Burak Emir
 * @param data content in this node, may not be null.
 */
class Unparsed(data: String) extends Atom[String](data)
{
  if (null == data)
    throw new IllegalArgumentException("tried to construct Unparsed with null")

  /** returns text, with some characters escaped according to XML spec */
  override def buildString(sb: StringBuilder) = sb append data
}

object Unparsed {
  def apply(data: String) = new Unparsed(data)
  def unapply(x: Unparsed) = Some(x.data)
}
