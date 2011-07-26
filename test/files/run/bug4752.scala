object Test {
  object Bippy {
    case object Dingus
  }

  def main(args: Array[String]): Unit = {
    assert(None.## == "None".##, None)
    assert(Test.Bippy.Dingus.## == "Dingus".##, Test.Bippy.Dingus)
  }
}
