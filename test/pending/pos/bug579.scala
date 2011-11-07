package my.webapp.bean.stuff;

import scala.reflect.BeanProperty

class MyBean {
  [BeanProperty]
  var frombulizer: String = _;
}

object Test extends Application {
 
  val x = new MyBean;
  x.frombulizer = "hello"

  x.setFrombulizer ("hola") // synthetic methods comes too late for typechecking this code

  val z:String = x.frombulizer

  val zz:String = x.getFrombulizer  // synthetic methods comes too late for typechecking this code

}
