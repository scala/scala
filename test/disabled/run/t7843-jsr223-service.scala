object Test extends App {
  val m = new javax.script.ScriptEngineManager()
  val engine = m.getEngineByName("scala")
  engine put ("n", 10)
  engine eval "1 to n.asInstanceOf[Int] foreach print"
}
