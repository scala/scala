object Test {

  def main(args: Array[String]) {
    args(0) match {
      case a: String => while(a == null) {}
    }
  }

}

