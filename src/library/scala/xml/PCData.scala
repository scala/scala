// $Id$

package scala.xml

/** This class (which is not used by all XML parsers, but always used by the XHTML one)
 *  represents parseable character data, which appeared as CDATA sections in the input
 *  and is to be preserved as CDATA section in the output.
 */
case class PCData(_data: String) extends Atom[String](_data) {
  /* The following code is a derivative work of scala.xml.Text */
  if (null == data)
    throw new IllegalArgumentException("tried to construct PCData with null")

  final override def equals(x: Any) = x match {
    case s: String  => s.equals(data)
    case s: Atom[_] => data == s.data
    case _ => false
  }

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ...
   */
  override def buildString(sb: StringBuilder) =
    sb append "<![CDATA[%s]]>".format(data)
}
