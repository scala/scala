
import java.lang.{Long => JLong}

class D {
  def f(x: Long, y: JLong): Long = x + y
  def f(x: JLong, y: JLong): Long = x + y
}
