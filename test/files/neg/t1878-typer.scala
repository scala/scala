object Test extends App {
  // illegal - bug #1764
  null match {
    case <p> { _* } </p> =>
  }
}
