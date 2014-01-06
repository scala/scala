import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext
import scala.collection.TraversableLike

// This one compiles
object Test {
  type Alias[T, CC[_]] = BlackboxContext { type PrefixType = TraversableLike[T, CC[T]] }
  def f() = macro f_impl
  def f_impl(c: Alias[Int, List])() = ???
}

// This one doesn't
object Test2 {
  type Ctx = scala.reflect.macros.BlackboxContext
  type Alias[T, CC[_]] = Ctx { type PrefixType = TraversableLike[T, CC[T]] }

  def f() = macro f_impl
  def f_impl(c: Alias[Int, List])() = ???
}
