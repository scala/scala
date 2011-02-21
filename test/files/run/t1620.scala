import java.io.PrintWriter
import scala.xml.XML
import scala.xml.dtd.{DocType, PublicID}

object Test extends App {
  val dt = DocType("foo", PublicID("-//Foo Corp//DTD 1.0//EN", "foo.dtd"), Seq())
  val pw = new PrintWriter(System.out)
  XML.write(pw, <foo/>, "utf-8", true, dt)
  pw.println()
  pw.flush()

  val dt2 = DocType("foo", PublicID("-//Foo Corp//DTD 1.0//EN", null), Seq())
  XML.write(pw, <foo/>, "utf-8", true, dt2)
  pw.println()
  pw.flush()
}
