import scala.language.dynamics

object MyDynamic extends Dynamic {
  def selectDynamic(name: String): Any = ???
}

object Test extends App {
  locally {
    import java.lang.String
    MyDynamic.id
  }
}
