package seal

sealed trait ISimpleValue

sealed trait IListValue extends ISimpleValue {
  def items: List[IAtomicValue[_]]
}
sealed trait IAtomicValue[O] extends ISimpleValue {
  def data: O
}

sealed trait IAbstractDoubleValue[O] extends IAtomicValue[O] { }
sealed trait IDoubleValue extends IAbstractDoubleValue[Double]

case class ListValue(val items: List[IAtomicValue[_]]) extends IListValue
class DoubleValue(val data: Double) extends IDoubleValue {
  def asDouble = data
}

object Test {
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val v: ISimpleValue = new DoubleValue(1)
    v match {
      case m: IListValue => println("list")
      case a: IAtomicValue[_] => println("atomic")
    }
  }
}
