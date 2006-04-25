object exc1 extends Application {
  def foo(): unit = {
    while (true) {
      try {
      } catch {
        case ex: Exception =>
      }
    }
  }
}
