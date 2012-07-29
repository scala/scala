class A { Self =>
  val ok = "ok"
  this match {
    case me@Self => println(me.ok)
  }
}

object Test extends A with App