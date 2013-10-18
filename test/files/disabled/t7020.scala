object Test {
  // warning was non-deterministic
  List(5) match {
    case 1 :: Nil | 2 :: Nil  =>
    case (x@(4 | 5 | 6)) :: Nil =>
    case 7 :: Nil  =>
    case Nil =>
  }

  List(5) match {
    case 1 :: Nil | 2 :: Nil  =>
    case (x@(4 | 5 | 6)) :: Nil =>
    case 7 :: Nil  =>
    case Nil =>
  }

  List(5) match {
    case 1 :: Nil | 2 :: Nil  =>
    case (x@(4 | 5 | 6)) :: Nil =>
    case 7 :: Nil  =>
    case Nil =>
  }

  List(5) match {
    case 1 :: Nil | 2 :: Nil  =>
    case (x@(4 | 5 | 6)) :: Nil =>
    case 7 :: Nil  =>
    case Nil =>
  }
}
