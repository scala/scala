object test {
  val x = if (2 == 3) (x: Int => 0) else (x: Int => "");
  val y: Int = x(2);
}
