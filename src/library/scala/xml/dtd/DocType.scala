/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml
package dtd

/** An XML node for document type declaration.
 *
 *  @author Burak Emir
 *
 *  @param  name   name of this DOCTYPE
 *  @param  extID  None, or Some(external ID of this doctype)
 *  @param  intSubset sequence of internal subset declarations
 */
case class DocType(name: String, extID: Option[ExternalID], intSubset: Seq[dtd.Decl])
{
  @deprecated("Use constructor that takes Option[ExternalID]", "2.10.0")
  def this(name: String, extID: ExternalID, intSubset: Seq[dtd.Decl]) =
    this(name, Some(extID), intSubset)

  if (!Utility.isName(name))
    throw new IllegalArgumentException(name+" must be an XML Name")

  /** returns "&lt;!DOCTYPE + name + extID? + ("["+intSubSet+"]")? >" */
  final override def toString() = {
    def intString =
      if (intSubset.isEmpty) ""
      else intSubset.mkString("[", "", "]")

    """<!DOCTYPE %s %s%s>""".format(name, extID.map(_.toString).getOrElse(""), intString)
  }
}

object DocType
{
  /** Creates a doctype with no extID, nor internal subset declarations. */
  def apply(name: String): DocType = apply(name, None, Nil)

  @deprecated("Use apply that takes Option[ExternalID]", "2.10.0")
  def apply(name: String, extID: ExternalID, intSubset: Seq[dtd.Decl]): DocType =
    apply(name, Some(extID), intSubset)
}
