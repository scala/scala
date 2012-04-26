import scala.reflect.mirror._

object Test extends App {
  def titi: Unit = {
    var truc = 0
    val tata = reify{() => {
      truc = truc + 6
    }}
    ()
  }
}
