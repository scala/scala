object Test extends Application {
  var xyz: (int, String, boolean) = _
    xyz = (1, "abc", true)
    Console.println(xyz)
    xyz._1 match {
      case 1 => Console.println("OK")
      case 2 => Console.println("KO")
      case 3 => Console.println("KO")
   }
}
