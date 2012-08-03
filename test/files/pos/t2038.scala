class Test {
  List(Some(classOf[java.lang.Integer]), Some(classOf[Int])).map {
    case Some(f: Class[_]) => f.cast(???)
  }
}