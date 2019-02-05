object Test {

  def main(args: Array[String]): Unit = {
    args(0) match {
      case a: String => while(a == null) {}
    }
  }

}

