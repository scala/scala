sealed trait ISimpleValue

sealed trait IListValue extends ISimpleValue
sealed trait IAtomicValue[O] extends ISimpleValue

sealed trait IAbstractDoubleValue[O] extends IAtomicValue[O]
sealed trait IDoubleValue extends IAbstractDoubleValue[Double]

case class ListValue(val items: List[IAtomicValue[_]]) extends IListValue
class DoubleValue(val data: Double) extends IDoubleValue

object Test extends App {
  // match is exhaustive
  (new DoubleValue(1): ISimpleValue) match {
    case m: IListValue => println("list")
    case a: IAtomicValue[_] => println("atomic")
  }
}
