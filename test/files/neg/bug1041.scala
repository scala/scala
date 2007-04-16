object test {
  (1 :: 2 :: Nil) match {
    case 1 => 4
    case _ => 0
  }
}
