import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait MappedRow
trait RowMapper[T <: MappedRow]
object RowMapper {
  implicit def mapper[T <: MappedRow]: RowMapper[T] = macro impl[T]
  def impl[T <: MappedRow : c.WeakTypeTag](c: Context) = c.universe.reify(new RowMapper[T]{})
}