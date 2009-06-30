@ImplementedBy(classOf[Provider])
trait Service {
  def someMethod()
}

class Provider
    extends Service
{
  // test enumeration java annotations
  @Ann(Days.Friday) def someMethod() = ()

  // #2103
  @scala.reflect.BeanProperty
  @Ann(value = Days.Sunday)
  val t2103 = "test"
}
