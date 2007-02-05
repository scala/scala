object Test extends Application {
  var xyz: {int, String, boolean} = _
  xyz = { 1, "abc", true }
  Console.println(xyz)
  xyz match {
    case { 1, "abc", true } => Console.println("OK")
  }
}
