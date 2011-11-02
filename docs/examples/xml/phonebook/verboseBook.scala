/* examples/xml/phonebook/verboseBook.scala */
package phonebook 

object verboseBook {

  import scala.xml.{ UnprefixedAttribute, Elem, Node, Null, Text, TopScope } 

  val pbookVerbose = 
    Elem(null, "phonebook", Null, TopScope,
       Elem(null, "descr", Null, TopScope,
            Text("This is a "), 
            Elem(null, "b", Null, TopScope, Text("sample")),
            Text("description")
          ),
       Elem(null, "entry", Null, TopScope,
            Elem(null, "name", Null, TopScope, Text("Burak Emir")),
            Elem(null, "phone", new UnprefixedAttribute("where","work", Null), TopScope, 
                 Text("+41 21 693 68 67"))
          )
       )

  def main(args: Array[String]) = 
    Console.println( pbookVerbose )
}
