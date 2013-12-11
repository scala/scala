trait DataSetup {
  type Memory <: AnyRef with Serializable
  def run(): Memory
}

object Use {

  val dataSetup = new DataSetup {     // <---- error reported here
    case class Mem(ids: List[Int])
    type Memory = Mem
    def run(): Memory = {
      val ids = List(1,2,3)
      Mem(ids)
    }
  }

}
