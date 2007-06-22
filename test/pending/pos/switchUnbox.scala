// this test has accompanying .flags file
//  that contains -Xsqueeze:on
//
object Foo {
  var xyz: (int, String) = (1, "abc")
  xyz._1 match {
    case 1 => Console.println("OK")
    case 2 => Console.println("OK")
    case _ => Console.println("KO")
  }
}
