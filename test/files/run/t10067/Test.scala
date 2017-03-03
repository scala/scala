object Test {
  def main(args: Array[String]): Unit = {
    //get inner class as some instance of super type
    var oc = new OuterClass();
    var icObj = oc.getInnerClassInstance();

    //get a stable identifier on outer class
    val ocStable = oc;

    //these will work
    icObj.isInstanceOf[ocStable.InnerClass];
    icObj.asInstanceOf[ocStable.InnerClass];

    //this will fail with java.lang.NoSuchMethodError
    icObj match {
      case ic: ocStable.InnerClass => ;
    }
  }
}
