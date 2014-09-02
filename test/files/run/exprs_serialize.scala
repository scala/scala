import java.io._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  def test(expr: Expr[_]) =
    try {
      val fout = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(fout)
      out.writeObject(expr)
      out.close()
      fout.close()

      val fin = new ByteArrayInputStream(fout.toByteArray)
      val in = new ObjectInputStream(fin)
      val reexpr = in.readObject().asInstanceOf[ru.Expr[_]].in(cm)
      in.close()
      fin.close()

      println(reexpr)
    } catch {
      case ex: Exception =>
        println(ex)
    }

  test(reify(2))
  test(reify{def foo = "hello"; foo + "world!"})
  test(reify {
    def foo(x: Int) = {
      class Local {
        val f = 2
      }
      val obj = new Local
      x % obj.f == 0
    }
    foo(5)
  })
}
