object Test extends Application {
  var xyz: Triple(int, String, boolean) = _
  xyz = Triple(1, "abc", true)
  Console.println(xyz)
  xyz match {
    case Triple(1, "abc", true) => Console.println("OK")
  }
}
