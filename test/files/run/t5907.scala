object Test extends App {
  t

  def t {
    val c1 = C1()(1)
    println(c1.copy()(2))

    {
      implicit val i = 2873
      println(c1.copy())
    }

    val c2 = C2()(1)
    println(c2.copy()(37))

    val c3 = C3(1,2)(3)
    println(c3.copy()(27))
    println(c3.copy(y = 22)(27))
    println(c3.copy(y = 7, x = 11)(27))

    val c4 = C4(1)
    println(c4.copy())
    println(c4.copy(x = 23))

    val c5 = C5(1,2)(3,"a")
    println(c5.copy()(33,"b"))
    println(c5.copy(y = 19)(33,"b"))

    {
      implicit val i = 193
      implicit val s = "c"
      println(c5.copy())
      println(c5.copy(y = 371))
      println(c5.copy(x = -1)(-2, "lken"))
    }

    val c6 = C6(1)(2)(3)
    println(c6.copy(29)(18)(-12))

    {
      implicit val i = 2892
      println(c6.copy(x = 1)(93))
      println(c6.copy(x = 1)(93)(761))
    }

    val c7 = C7(1)(2)(3)("h")
    println(c7.copy()(22)(33)("elkj"))

    {
      implicit val s = "me"
      println(c7.copy()(283)(29872))
      println(c7.copy(37)(298)(899)("ekjr"))
    }

    val c8 = C8(1)(2,3)()("els")
    println(c8.copy(x = 172)(989, 77)()("eliurna"))

    {
      implicit val s = "schtring"
      println(c8.copy()(82,2111)())
      println(c8.copy(x = -1)(92,29)()("lken"))
    }

    val c9 = C9(1)(2)()()("u")
    println(c9.copy()(271)()()("ehebab"))

    {
      implicit val s = "enag"
      println(c9.copy()(299))
      println(c9.copy()(299)())
      println(c9.copy()(299)()())
      println(c9.copy(x = -42)(99)()()("flae"))
    }

    class KA { override def toString = "ka" }
    class KB extends KA { override def toString = "kb" }
    val c10 = C10(10)(3)(19)
    println(c10.copy()(298)(27))
    println(c10.copy("elkn")("en")("emn"))
    println(c10.copy(new KA)(new KB)(new KB))

    {
      implicit val k = new KA
      println(c10.copy(new KA)(new KB))
    }
  }
}

case class C1(implicit x: Int) {
  override def toString = s"c1: $x"
}
case class C2()(y: Int) {
  override def toString = s"c2: $y"
}
case class C3(x: Int, y: Int)(z: Int) {
  override def toString = s"c3: $x, $y, $z"
}
case class C4(x: Int) {
  override def toString = s"c4: $x"
}
case class C5(x: Int, y: Int)(implicit z: Int, s: String) {
  override def toString = s"c5: $x, $y, $z, $s"
}
case class C6(x: Int)(y: Int)(implicit z: Int) {
  override def toString = s"c6: $x, $y, $z"
}
case class C7(x: Int)(y: Int)(z: Int)(implicit s: String) {
  override def toString = s"c7: $x, $y, $z, $s"
}
case class C8(x: Int)(y: Int, z: Int)()(implicit s: String) {
  override def toString = s"c8: $x, $y, $z, $s"
}
case class C9(x: Int)(y: Int)()()(implicit s: String) {
  override def toString = s"c9: $x, $y, $s"
}
case class C10[T,U <: T](x: T)(y: U)(implicit z: T) {
  override def toString = s"c9: $x, $y, $z"
}
