import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint:patmat,posterasure -Ystop-after:posterasure"

  override def code =
    """object A {
      |  def unapplySeq(a: Int) = new collection.SeqFactory.UnapplySeqWrapper(collection.mutable.ArrayBuffer(1,2,3))
      |}
      |object B {
      |  // this works: Some and immutable collections support the required interface.
      |  // in reqality, immutable collections also use the UnapplySeqWrapper value class, which is eliminated by erasure
      |  def unapplySeq(a: Int) = Some(collection.immutable.ArraySeq(1,2,3))
      |}
      |class Foo[T] {
      |  def isEmpty = false
      |  def get = this
      |  def apply(i: Int): T = ???
      |  def length = 2 // if there's no lengthCompare, the translation uses length
      |  def drop(n: Int): Seq[T] = ???
      |  def toSeq: Seq[T] = ???
      |}
      |object C {
      |  def unapplySeq(a: Int) = new Foo[Int]
      |}
      |object D {
      |  def unapplySeq[T](a: Int) = Some(new Foo[T])
      |}
      |object E {
      |  def unapplySeq(a: Int) = new Array.UnapplySeqWrapper(Array(1,2,3))
      |}
      |object F {
      |  def unapplySeq(a: Int) = Some("123")
      |}
      |class T {
      |  def t: Any = 2 match {
      |    case A(xs @ _*)    => xs      // wrapper.get.toSeq
      |    case A(x, y)       => (x, y)  // wrapper.get.apply(0/1)
      |    case A(x, xs @ _*) => (x, xs) // wrapper.get.drop(1)
      |    case B(xs @ _*)    => xs
      |    case B(x, y)       => (x, y)
      |    case B(x, xs @ _*) => (x, xs)
      |    case C(xs @ _*)    => xs
      |    case C(x, y)       => (x, y)
      |    case C(x, xs @ _*) => (x, xs)
      |    case D(xs @ _*)    => xs
      |    case D(x, y)       => (x, y)
      |    case D(x, xs @ _*) => (x, xs)
      |  }
      |}
    """.stripMargin

  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}
