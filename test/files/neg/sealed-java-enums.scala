import java.lang.Thread.State
import java.lang.Thread.State._

object Test {
  def f(state: State) = state match {
    case NEW | WAITING  => true
    case RUNNABLE       => false
    // and I forget the rest
  }
}
