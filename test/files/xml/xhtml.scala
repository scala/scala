// $Id$

import scala.xml.{Attribute, AttributeSeq, Node, Text};
import dtd._;

object Test {

  val n = Node.NoAttributes;

  def main( argv:Array[String] ) = {
    val link = A(
      AttributeSeq.fromAttrs(
        Attribute("","href","http://lampwww.epfl.ch")
      ),
      Text("link")
    );

    //val m = new scala.collection.mutable.HashMap[String, String];
    //m.put("href","http://lampwww.epfl.ch");
    //link.setAttribs(m);

    val body = Body(n,
      H1(n,Text("Welcome to xhtml in scala")),
      P(n,Text( "a paragraph")),
      P(n,Text("another one, with a "),link,Text(" to the LAMP homepage.")));
    val page = Html(n,
                    Head(n,
                         Base(
                           AttributeSeq.fromAttrs(
                             Attribute("","href","http://here.edu")
                           )),
                         Title(n,Text("a basic xhtml page"))),
                    body);
    System.out.println( page ) ;

    val doc = load(argv(0));
    System.out.println( doc );
  }

}
