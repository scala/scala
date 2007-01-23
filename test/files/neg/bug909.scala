case class Foo(x:Int)

object Bar {
        def main(args:Array[String]) {
                Foo(2) match {
                        case Foo("Hello") =>
                }
        }
}
