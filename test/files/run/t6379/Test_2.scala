import java.io._
import scala.reflect.runtime.universe._

class Reader(fname: String) {
  private val in = new BufferedReader(new FileReader(fname))
  @throws[IOException]("if the file doesn't exist")
  def read() = in.read()
}

object Test extends App {
  def test(sym: MethodSymbol): Unit = {
    println(s"uninitialized ${sym.name}: ${sym.exceptions}")
    sym.typeSignature
    println(s"initialized ${sym.name}: ${sym.exceptions}")
  }

  Macros.foo
  println("runtime")
  test(typeOf[Closeable].declaration(TermName("close")).asMethod)
  test(typeOf[Product1[_]].declaration(TermName("productElement")).asMethod)
  test(typeOf[Reader].declaration(TermName("read")).asMethod)
}
