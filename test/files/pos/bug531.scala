object Test extends Application {
  import scala.reflect._;
  def titi = {
    var truc = 0
    val tata: TypedCode[()=>Unit] = () => {
      truc = 6
    }
    ()
  }
}
