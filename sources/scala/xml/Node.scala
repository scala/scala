package scala.xml ;

/** trait for representation of XML elements. These are created by
**  a xxx2scala binding tool
**/
trait Node {

  def label:    String;
  def children: Seq[ Node ];

  def toXML: String;

}
