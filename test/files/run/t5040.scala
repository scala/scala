abstract class Prova2 extends Dynamic {
  def applyDynamic(m: String)(): Unit
  private def privateMethod() = println("private method")
}

object Test extends App {
  val prova= new Prova2 {
    def applyDynamic(m: String)() = println("applyDynamic")
  }
  prova.privateMethod()
}
