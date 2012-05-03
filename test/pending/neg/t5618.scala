



case class Class1


case class Class2(implicit class1: Class1)


object Test1 {
  val class2 = new Class2
  implicit val class1 = new Class1
}


object Test2 {
  val class2 = new Class2
  implicit val class1: Class1 = new Class1
}


object Test3 {
  implicit val class1 = new Class1
  val class2 = new Class2
}

