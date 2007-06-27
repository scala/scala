object TestExpressions {

  def patmatchScrut {
    lazy val z1: Option[String] = { println("forced <z1>"); Some("lazy z1") }

    val res = z1 match {
      case Some(msg) => msg
      case None => "failed"
    }
    print("lazy val in scrutinee: ")
    if (res == "lazy z1")
      println("ok")
    else
      println("failed")
  }

  def patmatchCase {
    val t: Option[String] = Some("test")
    val res = t match {
      case Some(msg) =>
        lazy val z1 = { println("forced <z1>"); "lazy z1" }
        z1

      case None => "failed"
    }
    print("lazy val in case: ")
    if (res == "lazy z1")
      println("ok")
    else
      println("failed")
  }


  def patmatchPat {
    lazy val Z1 = { println("forced <z1>"); "lazy Z1" }
    print("lazy val in case: ")
    val t: Option[String] = Some("lazy Z1")
    t match {
      case Some(Z1) =>
        println("ok")

      case None =>
        println("failed")
    }
  }

  def ifcond {
    lazy val z1 = { println("forced <z1>"); "lazy z1" }
    print("lazy val in if condition: ")
    if (z1 == "lazy z1")
      println("ok")
    else
      println("failed")
  }


  lazy val LazyField = { println("forced LazyField"); "LazyField" }

  def testPatMatchField {
    print("lazy val in pattern: ")
    val t: Option[String] = Some("LazyField")
    t match {
      case Some(LazyField) =>
        println("ok")

      case None =>
        println("failed")
    }
  }

  def test {
    patmatchScrut
    patmatchCase
    patmatchPat
    ifcond
    testPatMatchField
  }
}


object Test extends Application {

  TestExpressions.test
}
