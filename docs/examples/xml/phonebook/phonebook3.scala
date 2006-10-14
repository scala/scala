/* examples/xml/phonebook/phonebook3.scala */
package phonebook

object phonebook3 {

  import scala.xml.{Elem, Node, Text}
  import scala.xml.Utility.view
  import scala.xml.PrettyPrinter
  import Node.NoAttributes

  /* finds entry for Name, Where, changes it, or adds if not present */
  def change ( phonebook: Node, Name: String, Where: String, newPhone: String ) = {

    /** returns true if this element's first child is the right 'name'
     *  x is treated a if it was a singleton sequence here.
     */
    def hasName ( x: Seq[Node] ) = x(0).child.elements.next match {
      case <name>{ Text(Name) }</name> => true
      case _                           => false
    }

    /** returns true if this element has the right 'where' attribute
     *  y is treated a if it was a singleton sequence here.
     *  n.attribute(s) is the same as n.attributes.get(s)
     */
    def hasWhere ( y: Node ) = y.attribute("where") match {
      case Some(Text(Where)) => true
      case None              => false
    }

    /** returns true if this element has the right 'where' attribute
     *  the apply method of MetaData (n.attributes) returns raw a
     *  sequence of nodes.
     */
    def hasWhere2 ( y: Node ) = y.attributes("where") match {
      case null        => false
      case Text(Where) => true
      case _           => false
    }


    /** walks through tree, returns changed/copied updated tree  */
    def copyOrChange ( ch: Iterator[Node] ):List[Node] = {
        for( val c <- ch ) yield c match {

        case x @ <entry>{ ch1 @ _* }</entry> if hasName( x ) =>
          val it = ch1.elements;
          val nameElem:Seq[Node] = it.next;             // grab 'name' element
          val ch2 = nameElem concat copyOrChange( it ); // concat with updated seq

          if( ch1 == ch2 ) // not present: add as first entry

            <entry>
              <name>{ Name }</name>
              <phone where={ Where }>{ newPhone }</phone>
              { ch1 }
            </entry>

          else             // was present and changed

            <entry>
              { ch2 }
            </entry>

        case y @ <phone>{ _* }</phone> if hasWhere( y ) =>
            Console.println("c = "+c);
            <phone where={ Where }>{ newPhone }</phone>

          case _ =>
            Console.println("c = "+c);
            Console.println("c.attributes= "+c.attributes);
                            c

        }
    }.toList ; // for ... yield ... returns Iterator, convert to list

    // decompose phonebook, apply updates
    phonebook match {
      case <phonebook>{ ch @ _* }</phonebook> =>
        <phonebook>{ copyOrChange( ch.elements ) }</phonebook>
    }

  }

  val pb2 =
    change( phonebook1.labPhoneBook, "John", "work", "+41 55 555 55 55" );

  val pp = new PrettyPrinter( 80, 5 );

  def main( args:Array[String] ) = {
    Console.println("---before---");
    Console.println( pp.format( phonebook1.labPhoneBook ));
    Console.println("---after---");
    Console.println( pp.format( pb2 ));
  }
}
