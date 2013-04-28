import scala.language.higherKinds
import java.io._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  def test(tag: WeakTypeTag[_]) =
    try {
      val fout = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(fout)
      out.writeObject(tag)
      out.close()
      fout.close()

      val fin = new ByteArrayInputStream(fout.toByteArray)
      val in = new ObjectInputStream(fin)
      val retag = in.readObject().asInstanceOf[ru.WeakTypeTag[_]].in(cm)
      in.close()
      fin.close()

      println(retag)
    } catch {
      case ex: Exception =>
        println(ex)
    }

  def qwe[T, U[_]] = {
    test(implicitly[WeakTypeTag[T]])
    test(implicitly[WeakTypeTag[U[String]]])
  }

  qwe
}
