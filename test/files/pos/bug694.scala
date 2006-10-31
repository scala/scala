object test3 {
  trait Type[T];
  case object IntType extends Type[Int];
  case object StringType extends Type[String];

  def f[T](t : Type[T]) : T = t match {
    case IntType => 10;
    case StringType => "hello";
  }
}
