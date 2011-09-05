class Base(a: Any)

class Demo extends Base(new { Demo.this.toString }) {
  val x: Any = ()
}


class Demo2 extends Base(new { this.toString }) {
  val x: Any = ()
}

