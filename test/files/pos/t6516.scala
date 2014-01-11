import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.collection.TraversableLike

// This one compiles
object Test {
  type Alias[T, CC[_]] = Context { type PrefixType = TraversableLike[T, CC[T]] }
  def f() = macro f_impl
  def f_impl(c: Alias[Int, List])() = ???
}

// This one doesn't
object Test2 {
  type Ctx = scala.reflect.macros.blackbox.Context
  type Alias[T, CC[_]] = Ctx { type PrefixType = TraversableLike[T, CC[T]] }

  def f() = macro f_impl
  def f_impl(c: Alias[Int, List])() = ???
}
