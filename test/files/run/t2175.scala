case class Property[T](private var t: T) {
 var beforeChanges: List[(T, T) => Unit] = Nil
 var afterChanges: List[T => Unit] = Nil
 def apply = t
 def update(t2: T) = {
  beforeChanges foreach (_(t, t2))
  t = t2
  afterChanges foreach (_(t2))
 }
}

object Test
{
  def main(args: Array[String]): Unit = {
    val x = Property(10)
    x update 25
    assert(x.apply == 25)
  }
}

