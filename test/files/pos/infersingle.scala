object Test1 {
  def one[T](x: T): Option[T] = Some(x)
  val x = "one"
  val y: Option[x.type] = one(x)
}

object Test2 {
  // Has never worked, but seems desirable given the recent changes to
  // pattern type inference.
  val a = ""
  object Id {
    def unapply(xxxx: Any): Some[a.type] = Some[a.type](a)
  }
  val b: a.type = (a: a.type) match {
    case Id(x) => x
  }
}

object Test3 {
  val a = ""
  object Id {
    def unapply(xxxx: Any): Some[Test3.type] = Some[Test3.type](Test3)
  }
  val b: Test3.type = a match {
    case Id(x) => x
  }
}

class Test4 {
  val a = ""
  object Id {
    def unapply(xxxx: Any): Some[Test4.this.type] = Some[Test4.this.type](Test4.this)
  }
  val b: Test4.this.type = a match {
    case Id(x) => x
  }
}

class Super5 {
  final val q = ""
  def q1: q.type = q
}

class Test5 extends Super5 {
  val a = ""
  object Id {
    def unapply(xxxx: Any): Some[Test5.super.q.type] = Some[Test5.super.q.type](q1)
  }
  val b: Test5.super.q.type = a match {
    case Id(x) => x
  }
}
