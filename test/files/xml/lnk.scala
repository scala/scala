// $Id$

import scala.xml.{Attribute, AttributeSeq, Node};
import dtd._;

object Test {

  // 3 ways to construct your data
  def main(args:Array[String]) = {

    val n = Node.NoAttributes ;
    // construct data using original Element name
    //val b: Node = dtd._factory.get("link").match { case Some(x) => x(Nil,null)}; // safe
    // !!! System.out.println(b.toXML);

    // construct data using constructor (valid)
    val c = Link(
      new AttributeSeq(
        Attribute("","target","http://www.scala.org")
      ),
      Name(n, scala.xml.Text("hello-link"))
    );

    try {
      val c2 = Name(
        n,
        Link( n )
      );
      Console.println("eh ?");
    } catch {
      case scala.xml.dtd.ValidationException(msg) => {
        Console.print("validator throws exception: ");
        Console.println( msg );
      }
      case z => // ignore
        Console.println("whut??? "+z.getClass);

    }




    //c.getAttribs.update("target", "http://www.scala.org");
    System.out.println( c );

    // construct data using loading from a file
    val lnkDB = load(args(0));
    System.out.println( lnkDB );
  }

}
