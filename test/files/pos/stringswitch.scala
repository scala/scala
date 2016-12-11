package test;

object Switchtest {
	import scala.annotation.switch

	val foo = "foo"

	(foo: @switch) match {
		case "foo" => 1
		case "bar" => 2
		case "baz" => 3
		case "quuz" => 4
	}
}
