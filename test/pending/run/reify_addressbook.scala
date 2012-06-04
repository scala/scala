import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    case class Person(name: String, age: Int)

    /** An AddressBook takes a variable number of arguments
     *  which are accessed as a Sequence
     */
    class AddressBook(a: Person*) {
      private val people: List[Person] = a.toList

      /** Serialize to XHTML. Scala supports XML literals
       *  which may contain Scala expressions between braces,
       *  which are replaced by their evaluation
       */
      def toXHTML =
        <table cellpadding="2" cellspacing="0">
          <tr>
            <th>Name</th>
            <th>Age</th>
          </tr>
          { for (p <- people) yield
              <tr>
                <td> { p.name } </td>
                <td> { p.age.toString() } </td>
              </tr>
          }
        </table>;
    }

    /** We introduce CSS using raw strings (between triple
     *  quotes). Raw strings may contain newlines and special
     *  characters (like \) are not interpreted.
     */
    val header =
      <head>
        <title>
          { "My Address Book" }
        </title>
        <style type="text/css"> {
       """table { border-right: 1px solid #cccccc; }
          th { background-color: #cccccc; }
          td { border-left: 1px solid #acacac; }
          td { border-bottom: 1px solid #acacac;"""}
        </style>
      </head>;

    val people = new AddressBook(
      Person("Tom", 20),
      Person("Bob", 22),
      Person("James", 19));

    val page =
      <html>
        { header }
        <body>
         { people.toXHTML }
        </body>
      </html>;

    println(page)
  }.eval
}
