




object Constants {
  import scala.annotation.static
  @static val Const: Int = 0 // should generate a static final field
  @static final val FinalConst: Int = 0 // ditto
  @static var MutableField: Int = 0 // should not be final
}



