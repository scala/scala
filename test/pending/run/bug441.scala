object Test extends Application {

  def bug() = {
    val foo: Array[String] = Array("1","2","3");
    if( foo.length == null )    //  == 0 makes more sense, but still
      Console.println("plante"); // this code leads to runtime crash
    else
      Console.println("plante pas");
  }

  bug()
}
