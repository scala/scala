object Test extends App {
  println(Vector(1).view.updated(0,2).toList mkString ",")
  println(Seq(1,2,3).view.updated(2,8).toList mkString ",")
  println(List(1,2,3).view.updated(1,8).toList mkString ",")
}
