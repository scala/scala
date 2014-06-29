import scala.io.Source
import java.io.ByteArrayInputStream

object Test extends App {
  val txt = "abcdef"

  val in = new ByteArrayInputStream(txt.getBytes());
  val source = Source.fromInputStream(in);
  println(source.toString) // forces the BufferedSource to look at the head of the input

  println(source.mkString) // used to return "bcdef" ...
}
