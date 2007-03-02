package scala.xml

/** use this class to match on (unprefixed) attribute values
 *  <p>
 val hasName = new HasKeyValue("name")
 node match {
   case Node("foo", hasName(x), _*) => x // foo had attribute with key "name" and with value x
 }
 */
class HasKeyValue(key: String) {
        def unapplySeq(x:MetaData): Option[Seq[Node]] = x.get(key)
}

