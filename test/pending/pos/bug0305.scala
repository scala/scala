object Test extends Application {
 
  def foo(is:int*) = 1;
  def foo(i:int) = 2;

  Console.println( foo( List(3):_* ) )

}
