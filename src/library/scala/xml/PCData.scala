// $Id$

package scala.xml

/** This class (which is not used by all XML parsers, but always used by the XHTML one)
 *  represents parseable character data, which appeared as CDATA sections in the input
 *  and is to be preserved as CDATA section in the output.
 */
case class PCData(_data: String) extends Atom[String](_data) {
  if (null == data)
    throw new IllegalArgumentException("tried to construct PCData with null")

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ...
   */
  override def buildString(sb: StringBuilder) =
    sb append "<![CDATA[%s]]>".format(data)
}
