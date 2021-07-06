
final class Foo[T](val t: T) extends AnyVal

@res.Res(`type` = classOf[Foo[_]])
class It

object Test extends App {
  println(classOf[It].getAnnotation(classOf[res.Res]).`type`)
}
