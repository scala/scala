import scala.reflect.runtime.universe._

object Test extends App {
  def titi = {
    var truc = 0
    val tata = reify{() => {
      truc = 6
    }}
    ()
  }
}