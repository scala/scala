package my.webapp.bean.stuff;

import scala.reflect.BeanProperty

class MyBean {
  [BeanProperty]
  var frombulizer: String = _;
}

object Test extends Application {

  val x = new MyBean;
  x.frombulizer = "hello"

}
