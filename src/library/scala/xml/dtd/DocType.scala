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
case class DocType(name: String, extID: ExternalID, intSubset: Seq[dtd.Decl])
{
  if (!Utility.isName(name))
    throw new IllegalArgumentException(name+" must be an XML Name")

  /** returns "&lt;!DOCTYPE + name + extID? + ("["+intSubSet+"]")? >" */
  final override def toString() = {
    def intString =
      if (intSubset.isEmpty) ""
      else intSubset.mkString("[", "", "]")

    """<!DOCTYPE %s %s%s>""".format(name, extID.toString, intString)
  }
}
