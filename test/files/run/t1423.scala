object Test extends App{
  val x = 1 match {
    case 0xFFFFFFFF00000001L => println("Oops, overflow!");
    case 2L => println(2);
    case 1L => println(1);
    case _ => println("????");
  }
}
