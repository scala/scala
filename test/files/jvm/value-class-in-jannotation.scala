import javax.annotation.{Resource => R}

final class Foo[T](val t: T) extends AnyVal

@R(`type` = classOf[Foo[_]])
class It

object Test extends App {
  println(classOf[It].getAnnotation(classOf[R]).`type`)
}