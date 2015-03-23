object Test {
  final val a = ""
  var b: a.type = a
  b = a

  final val x = classOf[Object]
  var y: x.type = x
  y = x

  final val e = Thread.State.NEW
  var e1: e.type = e
  e1 = e
}
