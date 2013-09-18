package bippy {
  object Fail1 {
    def main(args: Array[String]): Any = ()
  }

  object Fail2 {
    def main[T](args: Array[String]): T = null.asInstanceOf[T]
  }

  abstract class Bippy[T] {
    def main(args: Array[String]): T = null.asInstanceOf[T]
  }
  object Fail3 extends Bippy[Unit] { }


  object Fail4 {
    def main(args: Array[String]): Unit = ()
  }
  trait Fail4 { }

  object Fail5 extends Fail5 { }
  class Fail5 {
    def main(args: Array[String]): Unit = ()
  }

  object Fail6 {
    def main(args: Array[String]): Unit = ()
  }
  class Fail6 {
    def main = "bippy"
  }

  object Win1 {
    def main(args: Array[String]): Unit = ()
  }
  object Win2 extends Bippy[Unit] {
    override def main(args: Array[String]): Unit = ()
  }
  trait WinBippy[T] {
    def main(args: Array[String]): T = null.asInstanceOf[T]
  }
  object Win3 extends WinBippy[Unit] { }
}

