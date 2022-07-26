
import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Vprint:cleanup"
  override def code = """
    class C {
      def f0 = Seq(Nil: _*)
      def f1 = Seq.apply[String](Nil: _*)

      def g = Seq.apply[String](scala.collection.immutable.Nil: _*)

      def h = scala.collection.immutable.Seq.apply[String](scala.collection.immutable.Nil: _*)

      def nil = Nil
    }
  """
  override def show() = assert(compile())
}

/*
was:
def f0(): Seq = scala.`package`.Seq().apply(scala.`package`.Nil()).$asInstanceOf[Seq]();
 */
