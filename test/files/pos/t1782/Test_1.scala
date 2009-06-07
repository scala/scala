@ImplementedBy(classOf[Provider])
trait Service {
  def someMethod()
}

class Provider
    extends Service
{
  def someMethod() = ()
}
