object Test {
  type K = Record.bip.T
  implicit val lk: List[K] = 1 :: Nil
  val r =  implicitly[List[K]]
}