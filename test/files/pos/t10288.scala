trait Target
trait Unrelated

class From
object From {
  import language.implicitConversions
  implicit def conv(f: From): Target = new Target {}
}

object Test {
  val works: Target = new From
  val breaks: Target = new From with Unrelated
  val breaksAlso: Target = new From {}

  def consumer(t: Target): Unit = ()
  consumer(new From) // works
  consumer(new From with Unrelated) // breaks
  consumer(new From {}) // breaks also
}