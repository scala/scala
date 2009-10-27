
object Test extends Application {

  object Color extends Enumeration {
    val Red, Green, Blue = Value
  }

  class MyColor extends Enumeration {
    val Red, Green, Blue = Value
  }

  println(Color.Red)
  println(Color.Green)
  println(Color.Blue)
  val col = new MyColor
  println(col.Blue)
  println(col.Green)
  println(col.Red)

}
