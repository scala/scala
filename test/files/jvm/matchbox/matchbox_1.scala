object Matchbox {
  import scala.{specialized => sp}

  def foo[@sp(Byte, Long) T](t: T): String = t match {
    case b: Byte => "byte " + b
    case l: Long => "long " + l
    case c       => "other " + c
  }

  def bar[@sp(Byte, Long) T](t: T): String =
    if (t.isInstanceOf[Byte]) "byte " + t.asInstanceOf[Byte]
    else if (t.isInstanceOf[Long]) "long " + t.asInstanceOf[Long]
    else "other " + t

  def baz[@sp(Byte, Long) T](t: T): String = {
    var mut = t
    val capturing = () => { println(mut); mut }
    mut match {
      case b: Byte => "byte " + b
      case l: Long => "long " + l
      case c => "other " + c
    }
  }

  /* should become iload_1; i2l; lload_2; ladd; lreturn */
  def quux(i: Int, l: Long): Long = (i: Any, l: Any) match {
    case (x: Int, y: Long) => x + y
    case _ => 20
  }
}