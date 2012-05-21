trait T extends Base_1 {
	def t1(as: String*): Array[String] = {
		varargs1(as: _*)
	}
	def t2(as: String*): Array[String] = {
    // This is the bug reported in the ticket.
		super.varargs1(as: _*)
	}
}

class C extends Base_1 {
	def c1(as: String*): Array[String] = {
		varargs1(as: _*)
	}
	def c2(as: String*): Array[String] = {
		super.varargs1(as: _*)
	}
}


object Test extends App {
	val t = new T {}
	println(t.t1("a", "b").mkString(","))
	println(t.t2("a", "b").mkString(","))

	val c = new C {}
	println(c.c1("a", "b").mkString(","))
	println(c.c2("a", "b").mkString(","))

}
