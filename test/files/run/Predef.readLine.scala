import java.io.StringReader
import scala.io.StdIn.readLine

object Test extends App {
  Console.withIn(new StringReader("")) {
    readLine()
    readLine("prompt\n")
    readLine("%s prompt\n", "fancy")
    readLine("%s %s prompt\n", "immensely", "fancy")
  }
}
