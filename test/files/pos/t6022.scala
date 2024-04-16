//> using options -Xfatal-warnings
//
class Test {
  (null: Any) match {
    case x: AnyRef if false =>
    case list: Option[_] =>
    case product: Product => // change Product to String and it's all good
    case x                => throw new MatchError(x)
  }
}
