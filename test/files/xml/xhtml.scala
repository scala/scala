// $Id$

import scala.xml.Text;
import dtd._;

object Test {

  def main( argv:Array[String] ) = {
    val link = A(Text("link"));

    //val m = new scala.collection.mutable.HashMap[String, String];
    //m.put("href","http://lampwww.epfl.ch");
    //link.setAttribs(m);

    val body = Body(
      H1(Text("Welcome to xhtml in scala")),
      P(Text( "a paragraph")),
      P(Text("another one, with a "),link,Text(" to the LAMP homepage.")));
    val page = Html(Head(Title(Text("a basic xhtml page"))), body);
    System.out.println(page.toXML) ;

    val doc = load(argv(0));
    System.out.println(doc);
    System.out.println(doc.toXML);
  }

}
