import java.io._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object Test extends App {
  def test(tag: TypeTag[_]) =
    try {
      val fout = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(fout)
      out.writeObject(tag)
      out.close()
      fout.close()

      val fin = new ByteArrayInputStream(fout.toByteArray)
      val in = new ObjectInputStream(fin)
      val retag = in.readObject().asInstanceOf[scala.reflect.basis.TypeTag[_]].in(cm)
      in.close()
      fin.close()

      println(retag)
    } catch {
      case ex: Exception =>
        println(ex)
    }

  test(implicitly[TypeTag[Int]])
  test(implicitly[TypeTag[String]])
}