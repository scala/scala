/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

// XXX This attempt to make Text not a case class revealed a bug in the pattern
// matcher (see ticket #2883) so I've put the case back.  (It was/is desirable that
// it not be a case class because it is using the antipattern of passing constructor
// parameters to the superclass where they become vals, but since they will also be
// vals in the subclass, it acquires an underscore to avoid a name clash.)
//
// object Text {
//   def apply(data: String) =
//     if (data != null) new Text(data)
//     else throw new IllegalArgumentException("tried to construct Text with null")
//
//   def unapply(other: Any): Option[String] = other match {
//     case x: Text  => Some(x.data)
//     case _        => None
//   }
// }

/** The class `Text` implements an XML node for text (PCDATA).
 *  It is used in both non-bound and bound XML representations.
 *
 *  @author Burak Emir
 *
 *  @param text the text contained in this node, may not be null.
 */
case class Text(_data: String) extends Atom[String](_data)
{
  if (_data == null)
    throw new IllegalArgumentException("tried to construct Text with null")

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ...
   */
  override def buildString(sb: StringBuilder) =
    Utility.escape(data, sb)
}
