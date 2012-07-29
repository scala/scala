object Test {
  Some(123).isInstanceOf[Option[Int]]
  Some(123).isInstanceOf[Option[String]]
  Some(123).isInstanceOf[Option[List[String]]]
  Some(123).isInstanceOf[Option[List[Int => String]]]
  Some(123).isInstanceOf[Option[(String, Double)]]
  Some(123).isInstanceOf[Option[String => Double]]
}
