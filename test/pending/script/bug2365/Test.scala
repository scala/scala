import scala.tools.nsc.io._
import java.net.URL

object A { def apply(d: { def apply(): Int}) = d.apply() }
object A2 { def apply(d: { def apply(): Int}) = d.apply() }
object A3 { def apply(d: { def apply(): Int}) = d.apply() }
object A4 { def apply(d: { def apply(): Int}) = d.apply() }

class B extends Function0[Int] {
	def apply() = 3
}

object Test
{
  type StructF0 = { def apply(): Int }
	def main(args: Array[String]) {
		for(i <- 0 until 150)
			println(i + " " + test(A.apply) + " " + test(A2.apply) + " " + test(A3.apply) + " " + test(A3.apply))
	}
	
	def test(withF0: StructF0 => Int): Int = {
	  // Some large jar
	  val jar = File("../../../../lib/scalacheck.jar").toURL
	  // load a class in a separate loader that will be passed to A
	  val loader = new java.net.URLClassLoader(Array(File(".").toURL, jar))
	  // load a real class to fill perm gen space
    Class.forName("org.scalacheck.Properties", true, loader).newInstance 
    // create a class from another class loader with an apply: Int method
  	val b = Class.forName("B", true, loader).newInstance
	
		// pass instance to a, which will call apply using structural type reflection.
		// This should hold on to the class for B, which means bLoader will not get collected
	  withF0(b.asInstanceOf[StructF0])
	}
}
