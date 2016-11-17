class Context(val v : AnyRef)

trait AbidePlugin {
  val someVal = ""

       val x = null.asInstanceOf[Context { val v : someVal.type }] // CRASH
  lazy val y = null.asInstanceOf[Context { val v : someVal.type }] // CRASH
       var z = null.asInstanceOf[Context { val v : someVal.type }] // CRASH
}

class C {
  val someVal = ""

       val x = null.asInstanceOf[Context { val v : someVal.type }]
  lazy val y = null.asInstanceOf[Context { val v : someVal.type }] // CRASH
       var z = null.asInstanceOf[Context { val v : someVal.type }]
}
