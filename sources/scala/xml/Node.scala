package scala.xml ;

/** superclass for specific representation of XML elements. These are created by
**  a xxx2scala binding tool
**/
abstract class Node {

  def label:    String;
  def children: Seq[ Node ];

  def toXML: String;

}
