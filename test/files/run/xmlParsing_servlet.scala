import scala.xml._ ;

object Test {

  val headerMsg = Text("What follows is an example of modular formatting.");
  val footerMsg = Text("Complicated layout tasks can be encapsulated and outsourced.");

  /** helper function for the main page, provides header and a footer
   */
  def page( ns:Seq[Node] ) = {
    <html>
      <head>
        <title>ModularFormatting</title>
      </head>
      <body>
        <h2>Welcome</h2>
        <p>
          { headerMsg }
        </p>
        <p>
          { ns:_* }
        </p>
        <hr/>
        <p>
          { footerMsg }
        </p>
        <h2>Bye!</h2>
      </body>
    </html>
  }

  /** applies beautify to every element in a sequence
   */
  def beautify( xs:Seq[Node] ):Seq[Node] = xs.toList.map { beautify }

  /** this is a recursive procedure that adds some attributes to the tree
   */
  def beautify( n:Node ):Node = n match {
    case <td>{ xs @ _* }</td> =>
          <td bgcolor="#AAAAFF" color="#222255">{ xs:_* }</td>

    case <table>{ xs @ _* }</table> =>
          <table align="center">{ beautify( xs ):_* }</table>

    case Elem( label, xs @ _* ) =>
          Elem( label, beautify( xs ):_*)

    case _ => n
  }

  /** this function will take a node and put it in a table
   */
  def format( msg:Node ):Node = {
    <table>
      <tr>
        <td>{ msg }</td>
      </tr>
    </table>
  }

  /** returns a highlighted text node with the string given as arguemnt. if it
   *  is null, supplies a default string.
   */
  def getMessage( x:String ) = {
    if( x == null )
      <h1> This could be YOUR message ! </h1>
    else
      <h1> { Text( x ) } </h1>
  }

  /** the entry method
   */
  def doGetXML() = {
    beautify( page( List( format( getMessage( "message" ) )) ));
    /* page( List( format( theMessage ))); */

  }

  def main( args:Array[String] ) = {
    Console.println( doGetXML() );
  }

}
