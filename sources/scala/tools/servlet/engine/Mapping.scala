package scala.tools.servlet.engine;

import java.io._;

class Mapping(){

  val map = new File(http.HTTP.SERVER_LOCATION,http.HTTP.translateFilename("map.prop"));
  val myProp = new java.util.Properties();

  try{
    myProp.load(new FileInputStream(map));
  }

  catch{
    case e:FileNotFoundException  =>  System.out.println("FileNotFoundException ");
    case e:IOException =>  System.out.println("IOException");
    case e: IllegalArgumentException =>  System.out.println(" IllegalArgumentException");
  }

  def switch(original:String):String={
    myProp.getProperty(original);
  }

}
