class C {
  final def resume: Unit = (this: Any) match {
    case x : C => (x: Any) match {
      case y : C =>
        () => (x, y) // used to trigger a ClassFormatError under -Ydelambdafy:method
    }
  }
}

object Test extends App {
  new C().resume
}
