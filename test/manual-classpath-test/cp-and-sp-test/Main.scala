import c.A
import f.g.D
import h.i.E

object Main extends App {
	println(new A().foo.map(_.toString + "/" + D(E("text"))))
}