package test;

object Switchtest {
	import scala.annotation.switch

	val bar = "bar"

	val num = (bar: @switch) match {
		case "foo" => 1
		case "bar" => 2
		case "baz" => 3
		case "quuz" => 4
	}

	println("!!!!!! $num !!!!")
}
