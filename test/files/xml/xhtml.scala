// $Id$

import scala.xml.PCDATA;
import dtd._;

object Test {

  def main( argv:Array[String] ) = {
    val link = A(PCDATA("link"));

    val m = new scala.xml.javaAdapter.HashMap[String, String];
    m.put("href","http://lampwww.epfl.ch");
    link.setAttribs(m);

    val body = Body(
      H1(PCDATA("Welcome to xhtml in scala")),
      P(PCDATA( "a paragraph")),
      P(PCDATA("another one, with a "),link,PCDATA(" to the LAMP homepage.")));
    val page = Html(Head(Title(PCDATA("a basic xhtml page"))), body);
    System.out.println(page.toXML) ;

    val doc = load(argv(0));
    System.out.println(doc);
    System.out.println(doc.toXML);
  }

}
