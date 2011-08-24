class Base(a: Any)

class Demo extends Base(new { Demo.this }) {
  val x: Any = ()
}


class Demo2 extends Base(new { this }) {
  val x: Any = ()
}

