package runscala

object MainScala {
	def main(args: Array[String]): Unit = ()

  object StaticInner {
	  def main(args: Array[String]): Unit = ()
  }
}

class NoStatic {
	def main(args: Array[String]): Unit = ()
}

class NoArgs {
  def main(): Unit = ()
}

class Protected {
  protected def main(): Unit = ()
}

trait Tr {
  def main(): Unit = ()
}

abstract class Abs {
  def main(): Unit = ()
}

class InheritTrait extends Tr
abstract class AbsInheritAbs extends Abs

class NonVoid {
  def main(): Int = 1
}
