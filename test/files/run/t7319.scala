import scala.tools.partest.ReplTest

object Test extends ReplTest {
  // so we can provide the ambiguities, rather than relying in Predef implicits
  override def extraSettings = "-Yno-predef"
  override def code = """
class M[A]
implicit def ma0[A](a: A): M[A] = null
implicit def ma1[A](a: A): M[A] = null
def convert[F[X <: F[X]]](builder: F[_ <: F[_]]) = 0
convert(Some[Int](0))
Range(1,2).toArray: Seq[_]
0""" // before the fix, this line, and all that followed, re-issued the implicit ambiguity error.
}
