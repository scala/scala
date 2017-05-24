object Test extends (() => Int) {
  def apply(): Int = 1
  final override val toString = "Foo";
  def main(args: Array[String]): Unit = println(this)
}
