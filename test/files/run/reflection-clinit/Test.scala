import reflect.runtime.universe._

object Test {
  def main(args: Array[String]): Unit = {
    typeOf[p1.A] // used to call C.<clinit>
  }
}

