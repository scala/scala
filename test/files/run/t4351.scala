object Test {
  def main(args: Array[String]): Unit = {
    try new BooleanPropImpl().value
    catch {
      // was: StackOverflowError
      case e: RuntimeException => println("runtime exception")
    }
  }
}

trait Prop[@specialized(Boolean) +T] {
  def value: T
}

class PropImpl[+T] extends Prop[T] {
  def value: T = scala.sys.error("")
}

trait BooleanProp extends Prop[Boolean]

class BooleanPropImpl() extends PropImpl[Boolean] with BooleanProp
