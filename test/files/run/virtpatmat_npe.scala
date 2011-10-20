class C {
  class D
  val values = new Array[AnyRef](10)
  values(0) match {
    case name: D => println("NOK: "+ name) // the outer check on D's outer should not cause a NPE
    case null   => println("OK")
  }
}

object Test extends C with App