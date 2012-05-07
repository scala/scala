import language.dynamics

class DynamicTest extends Dynamic {
  def updateDynamic(name: String)(value: Any) {
    println(s"You have just updated property '$name' with value: $value")
  }
}

object MyApp extends App {
  def testing() {
    val test = new DynamicTest
    test.firstName = "John"
  }

  testing()
}