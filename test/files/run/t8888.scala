//> using options -Ydelambdafy:method
//
class C {
  final def resume: Any = (this: Any) match {
    case x : C => (x: Any) match {
      case y : C =>
        () => (x, y) // used to trigger a ClassFormatError under -Ydelambdafy:method
      case y => throw new MatchError(y)
    }
    case x => throw new MatchError(x)
  }
}

object Test extends App {
  new C().resume
}
