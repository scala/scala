object Test {

  def main(args: Array[String]) {

    var coll = {
      val key = 4
      try   { null }
      catch { case _ => null }
    }

    { () => coll } // the purpose of this is making `coll` an ObjectRef

  }
}
