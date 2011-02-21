
/** Test scala.Console functionality. */
object Test extends App {

  import Console._
  print(true)
  print(1)
  print(1.0)
  flush
  println("..")
  println(1)
  printf("Argument nr. %d has value %1.2f\n",
         1, 10.0/3)
}
