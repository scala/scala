// $Id$

import scala.util.continuations._

object Test {

  def bar() = shift { k: (String => String) => k("1") }
 
  def foo() = reset { bar(); 7 }
        
}
