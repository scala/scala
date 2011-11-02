object Test extends App{
  val x = (1 : Byte) match {
    case 2 => println(2);
    case 1 => println(1);
    case _ => println("????");
  }
} 
