class JC_1 {

  def m(a: Option[String]) {
    a match {
      case null => println("null")
      case None => println("None")
      case Some(x) if x == null => println("Some(null)")
      case Some(x)              => println(s"Some($x)")
    }
  }

}
