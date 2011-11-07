object Test {
  abstract class Base { val changesBaseClasses: Boolean }
  class Concrete extends Base { val changesBaseClasses = true }
  def getBase : Base = new Concrete
  
  var c = new Base { val changesBaseClasses = true }
  c = getBase
}
