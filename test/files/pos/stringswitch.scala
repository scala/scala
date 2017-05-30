package test;

object Switchtest {
	import scala.annotation.switch

	def getnum(str: String) = (str: @switch) match {
		case "foo" => 1
		case "bar" => 2
		case "baz" => 3
		case "quuz" => 4
		case _ => 0
	}

    def main( args:Array[String] ): Unit = {

      println(getnum("none"))
	  println(getnum("foo"))
	  println(getnum("baz"))
	  println(getnum("bar"))
	  println(getnum("quuz"))
    }

	
}
