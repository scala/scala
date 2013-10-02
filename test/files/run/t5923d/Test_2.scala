class RowA extends MappedRow
class RowB extends MappedRow

object Test extends App {
  implicitly[RowMapper[RowA]]
  implicitly[RowMapper[RowB]]
}