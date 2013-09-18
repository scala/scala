trait T extends Any {

  var x = 1

  { x += 1 }

  type T = Int

  val y: T
}
