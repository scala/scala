object Test extends App {
  println((1, 2, 3) match { case (r, \u03b8, \u03c6) => r + \u03b8 + \u03c6 })
  println(1 match { case \u00e9 => \u00e9 })
}
