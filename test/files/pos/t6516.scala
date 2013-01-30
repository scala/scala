import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.TraversableLike

// This one compiles
object Test {
  type Alias[T, CC[_]] = Context { type PrefixType = TraversableLike[T, CC[T]] }
  def f() = macro f_impl
  def f_impl(c: Alias[Int, List])() = ???
}

// This one doesn't
object Test2 {
  type Ctx = scala.reflect.macros.Context
  type Alias[T, CC[_]] = Ctx { type PrefixType = TraversableLike[T, CC[T]] }

  def f() = macro f_impl
  def f_impl(c: Alias[Int, List])() = ???
}
