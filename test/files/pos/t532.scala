object Test extends App {
  import scala.reflect._;
  def titi: Unit = {
    var truc = 0
    val tata = Code.lift{() => {
      truc = truc + 6
    }}
    ()
  }
}
