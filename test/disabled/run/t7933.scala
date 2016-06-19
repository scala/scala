object Test extends App {
  val m = new javax.script.ScriptEngineManager()
  val engine = m.getEngineByName("scala")
  val res2 = engine.asInstanceOf[javax.script.Compilable]
  res2 compile "8" eval()
  val res5 = res2 compile """println("hello") ; 8"""
  res5 eval()
  res5 eval()
}
