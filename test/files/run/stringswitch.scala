object Test extends App {
	import scala.annotation.switch

	def getnum(str: String) = (str: @switch) match {
		case "foo" => 1
		case "bar" => 2
		case "baz" => 3
		case "quuz" => 4
		case _ => 0
	}

    println(getnum("none"))
    println(getnum("foo"))
	println(getnum("baz"))
	println(getnum("bar"))
	println(getnum("quuz"))

}
