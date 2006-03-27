object Test extends Application {
  import scala.reflect._;
  def titi: Unit = {
    var truc = 0
    val tata: Code[()=>Unit] = () => {
      truc = truc + 6
    }
    ()
  }
}
