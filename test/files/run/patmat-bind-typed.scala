object Test {
  def f(xs: List[Any]) = for (key @ (dummy: String) <- xs) yield key

  def main(args: Array[String]): Unit = {
    f("abc" :: Nil) foreach println
    f(5 :: Nil) foreach println
  }
}
