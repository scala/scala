import scala.xml.Utility.sort

object Test extends App {
  println(sort(<a/>))
  println(sort(<a d="1" b="2" c="3"/>))
  println(sort(<a d="1" b="2" e="3" c="4" f="5"/>))
  println(sort(<a f="1" e="2" d="3" c="4" b="5"/>))
  println(sort(<a b="1" c="2" d="3" e="4" f="5"/>))

  println(sort(<a a:d="1" a:b="2" a:c="3"/>))
  println(sort(<a a:d="1" a:b="2" a:e="3" a:c="4" a:f="5"/>))
  println(sort(<a a:f="1" a:e="2" a:d="3" a:c="4" a:b="5"/>))
  println(sort(<a a:b="1" a:c="2" a:d="3" a:e="4" a:f="5"/>))
}

