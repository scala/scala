//> using options -Xfatal-warnings
object MyEnum extends Enumeration {
  val e1 = Value("e1")
  val e2 = Value("e2")
}

object MyOtherEnum extends Enumeration {
  val e3 = Value("e3")
  val e4 = Value("e4")
}

object Test {
  def fn(e: MyEnum.Value) = e match {
    case MyEnum.e1 => "1"
    case MyEnum.e2 => "2"
  }

  def fn2(e: MyEnum.Value, o: MyOtherEnum.Value) = (e, o) match {
    case (MyEnum.e1, MyOtherEnum.e3) => "4"
    case (MyEnum.e1, MyOtherEnum.e4) => "5"
    case (MyEnum.e2, MyOtherEnum.e3) => "6"
    case (MyEnum.e2, MyOtherEnum.e4) => "7"
  }
}
