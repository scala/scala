object Foo extends App{
  'f' match {
    case 'o'|'c'|'b' => println("Oooo");
    case _ => println("stuff");
    case 'f' => println("not stuff?");
  }

}
