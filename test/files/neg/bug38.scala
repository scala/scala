object testElem {

  import scala.xml.* ; // does not work
  //import scala.xml.Element ; //works

  def main( args:Array[String] ) = {
      val foo = new Element { def getName = "hallo"; def getChildren = Nil; def getAttribs = new HashMap[String,String] };
      ()
  }

}
