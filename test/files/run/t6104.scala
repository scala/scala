class A { Self =>
  val ok = "ok"
  this match {
    case me@Self => println(me.ok)
    case x       => throw new MatchError(x)
  }
}

object Test extends A with App
