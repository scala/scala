package scala.xml.parsing;

import scala.collection.Map ;
import scala.collection.{ immutable, mutable };

/** This handler is adapted to the foul handling of namespaces used in
 *  W3C XML Schema, notably, that the prefixes of namespace declarations
 *  in the root element are referenced from attribute values.
 */
class XSDHandler extends ConstructingHandler {

  /** namespace prefix map of the root element */
  var prefixes: immutable.Map[String,String] = null;

  override def internal_startPrefixMapping:Unit = {
    if( prefixes == null )
      prefixes = new immutable.TreeMap.Empty[String,String] incl tmpPrefix;
    super.internal_startPrefixMapping
  }

}
