// $Id$

import scala.xml.PCDATA;
import scala.xml.Element;
import dtd._;

object Test {

  // 3 ways to construct your data
  def main(args:Array[String]) = {
    // construct data using original Element name
    val b: Element = dtd._factory.get("link")(Nil); // safe
    // !!! System.out.println(b.toXML);

    // construct data using constructor
    val c = Link(Name(PCDATA("hello-link")));
    c.getAttribs.put("target", "http://www.scala.org");
    System.out.println(c.toXML);

    // construct data using loading from a file
    val lnkDB = load(args(0));
    System.out.println(lnkDB.toXML);
  }

}
