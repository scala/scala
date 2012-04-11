import scala.reflect.mirror._

object Test extends App {
  {
    val code = reify {
      var x = 2
      def y = x // forcibly captures x
      reify{x}.eval
    }
    println(code.eval)
  }
}