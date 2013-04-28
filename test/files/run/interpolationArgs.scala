object Test extends App {
  try { scala.StringContext("p1", "p2", "p3").s("e1") } catch { case ex: Throwable => println(ex) }
  try { scala.StringContext("p1").s("e1") } catch { case ex: Throwable => println(ex) }
}

