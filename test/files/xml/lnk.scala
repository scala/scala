// $Id$

import scala.xml.Node;
import dtd._;

object Test {

  // 3 ways to construct your data
  def main(args:Array[String]) = {

    val n = Node.NoAttributes ;
    // construct data using original Element name
    //val b: Node = dtd._factory.get("link").match { case Some(x) => x(Nil,null)}; // safe
    // !!! System.out.println(b.toXML);

    // construct data using constructor
    val c = Link(n + "target" -> "http://www.scala.org", Name(n, scala.xml.Text("hello-link")));
    //c.getAttribs.update("target", "http://www.scala.org");
    System.out.println( c );

    // construct data using loading from a file
    val lnkDB = load(args(0));
    System.out.println( lnkDB );
  }

}
