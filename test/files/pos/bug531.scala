object Test extends App {
  import scala.reflect._;
  def titi = {
    var truc = 0
    val tata: Code[()=>Unit] = () => {
      truc = 6
    }
    ()
  }
}
