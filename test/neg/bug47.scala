// this is not possible

object Test {

  class Elem;
  class MySuperXML_Tag( children: Elem* ) extends Elem;

  val factory = new HashMap( String, Elem* => Elem )
  factory.put("MySuperXML_Tag", MySuperXML_Tag )
//                            ^
// error "methods with repeated args have to be
//        fully applied"
}
