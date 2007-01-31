
/** Test scala.Console functionality. */
object Test extends Application {

  import Console._
  print(true)
  print(1)
  print(1.0)
  flush
  println("..")
  println(1)
  printf("Argument nr. {0,number} has value {1,number,#.##}\n",
         1, 10.0/3)
}
