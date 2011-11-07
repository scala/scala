import java.util.ArrayList

trait Bind[Z[_]] 

class MySerializable[X] extends java.io.Serializable

object Bind {
	implicit val JavaArrayListBind: Bind[ArrayList] = new Bind[ArrayList] {}
	implicit val MySerializableBind: Bind[MySerializable] = new Bind[MySerializable] {}
}

object works {
	// this works fine:
	def runbind(implicit bind: Bind[MySerializable]) {}
	runbind
}

object breaks {
	def runbind(implicit bind: Bind[ArrayList]) {}
	runbind  
	/*java.lang.AssertionError: assertion failed: java.io.Serializable
		at scala.Predef$.assert(Predef.scala:107)
		at scala.tools.nsc.symtab.Types$TypeRef.transform(Types.scala:1417)
		at scala.tools.nsc.symtab.Types$TypeRef.baseType(Types.scala:1559)
	*/
}
