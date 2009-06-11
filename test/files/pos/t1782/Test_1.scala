@ImplementedBy(classOf[Provider])
trait Service {
  def someMethod()
}

class Provider
    extends Service
{
  // test enumeration java annotations
  @Ann(Days.Friday) def someMethod() = ()
}
