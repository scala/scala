object test {

    abstract class Base() {
      val x: String;
      val y = 1.0;
    }

    case class Sub() extends Base() {
      val x = "hello";
      override val y = 2.0;
    }

    abstract class Sub2() extends Base() {
      override val Pair(x, y) = Pair("abc", 2.0);
    }
}
