package test

object A {
  println(("a" match {
    case "a" => 1
    case _ => "a"
  }).asInstanceOf[Object])
}
