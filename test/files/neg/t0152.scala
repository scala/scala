class Value[+T](x: T) {
    def value = x
}

trait PlusOne extends Value[Int] {
    override def value = super.value + 1
}

object Test extends App {
 object boom extends Value[java.lang.String]("foo") with PlusOne
       
 println(boom.value) // class cast exception!
}
