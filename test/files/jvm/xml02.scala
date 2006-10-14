object Test {
def main(args:Array[String]) = {
  import scala.xml.NodeSeq
  import NodeSeq.view
  import testing.UnitTest._


  val ax = <hello foo="bar">
             <world/>
           </hello>

  Console.println("attributes");

  Console.println("one");
  assertEquals(ax \ "@foo", "bar")
  Console.println("two");
  assertEquals(ax \ "@foo", xml.Text("bar"))

  val bx = <hello foo="bar&amp;x"></hello>

  Console.println("three");
  assertEquals(bx \ "@foo", "bar&x")
  Console.println("four");
  assertSameElements(bx \ "@foo", List(xml.Text("bar&x")))
  //assertSameElements(bx \ "@foo", List(xml.Text("bar"),xml.EntityRef("amp"),xml.Text("x")))

  Console.println("five");
  assertEquals(bx.toString, "<hello foo=\"bar&amp;x\"></hello>")


  /* patterns */
  Console.println("patterns");
  assertEquals(<hello/> match { case <hello/> => true; case _ => false; },
               true);



  /*
  assertEquals(ax match { case x @ <hello>
                               <world/>
                           </hello> if x \ "@foo" == "bar" => true;
                      case _ => false; },
               true);

  assertEquals(
     <hello foo="bar">
       crazy text world
     </hello> match { case <hello>
                               crazy   text  world
                           </hello> => true;
                      case _ => false; },
               true);
  */
}

}
