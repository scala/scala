package files.pos

case class CaseClass(intData : Int, boolData : Boolean)
object CaseClass{
  def apply(intData: Int, boolData: Boolean): CaseClass = new CaseClass(intData, boolData)

  def unapply(arg: CaseClass): scala.Option[Int] = Some(arg.intData)
}

object CustomCaseClassUnapply{

  def main(args : Array[String]): Unit = {
    val caseClass : CaseClass = CaseClass(1,false)
    caseClass match {
      case CaseClass(intData) => intData
    }
  }

}