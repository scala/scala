object Test {
  final val a = ""
  var b: a.type = a
  b = a

  final val x = classOf[Object]
  var y: x.type = x
  y = x
}
