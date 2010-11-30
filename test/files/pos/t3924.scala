object Test {
  class Hoe extends Serializable {
    def add(a: java.io.Serializable): Unit = println(a)
    def someMethod() { add(this) }
  }
}
