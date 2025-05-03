import scala.concurrent._

trait TestModule {
  implicit def ec: ExecutionContext
}

class ClassA()(implicit ec: ExecutionContext)

trait Trait1 {
  this: TestModule =>

  implicit def ec: ExecutionContext
  def a: ClassA = new ClassA()
  def b = new ClassA()
}

class K extends Trait1 with TestModule {
  override implicit def ec: ExecutionContext = ???
}

object Test extends App {
  println {
    new K().a
  }
}
