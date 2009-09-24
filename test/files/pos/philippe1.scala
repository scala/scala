object test {
  def id[a](xs: Array[a]): Array[a] = xs;

  def main(args: Array[String]): Unit = {
      val res: Array[String] = id(args);
      ()
  }
}
