import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

trait MappedRow
trait RowMapper[T <: MappedRow]
object RowMapper {
  implicit def mapper[T <: MappedRow]: RowMapper[T] = macro impl[T]
  def impl[T <: MappedRow : c.WeakTypeTag](c: BlackboxContext) = c.universe.reify(new RowMapper[T]{})
}