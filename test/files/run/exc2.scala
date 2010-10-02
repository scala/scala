object Test extends Application {
  def foo() = {
    while (true) {
      try {
        Console.println("foo")
      } catch {
        case ex: Exception =>
          Console.println("bar")
      }
    }
  }
}
