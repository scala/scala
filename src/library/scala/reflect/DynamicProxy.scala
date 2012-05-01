package scala.reflect
/**
 * A dynamic proxy which redirects method calls and attribute access to a given
 * target object at runtime using reflection.
 *
 * Usage example:
 *
 *  object x{ def hello = "hello world" }
 *  val d = new DynamicProxy{ val dynamicProxyTarget = x }
 *  assert( d.hello == "hello world" )
 *
 * Not supported (yet):
 *   - implicit conversions and parameters
 *   - multiple arguments lists
 *   - explicit type arguments
 */
trait DynamicProxy extends Dynamic{
  /** Method calls on DynamicProxy are redirected to this object. Needs to be defined in a subclass. */
  val dynamicProxyTarget : AnyRef

  import scala.reflect.mirror._
  /**
   * boxing to preserve information on primitive types for overloading resolution
   */
  case class DynamicReflectBoxed( class_ : Class[_], value: Any )
  object DynamicReflectBoxed{
    implicit def box[@specialized T]( v:T ) = DynamicReflectBoxed( v.getClass, v )
  }

  def selectDynamic( method:String ) = {
    val symbol = classToType( dynamicProxyTarget.getClass ).member( newTermName(method).encodedName )
    invoke( dynamicProxyTarget, symbol )()
  }

  def updateDynamic( method:String )( value : Any ) = {
    val symbol = classToType( dynamicProxyTarget.getClass ).member( newTermName(method+"_=").encodedName )
    invoke( dynamicProxyTarget, symbol )( value )
  }

  def applyDynamic( method:String )( args:DynamicReflectBoxed* ) : Any
    = applyDynamicNamed( method )( args.map( value => ("",value) ) :_* )

  def applyDynamicNamed( method:String )( args:(String,DynamicReflectBoxed)* ) : Any = {
    val class_ = dynamicProxyTarget.getClass
    var i = 0
    val toolbox = mkToolBox(mkConsoleFrontEnd(),"")
    val symbol = classToType( dynamicProxyTarget.getClass ).member( newTermName(method).encodedName )
    if(args.size == 0){
      invoke( dynamicProxyTarget, symbol )()
    } else {
      val call =
        Apply(
          Select(
            TypeApply(
              Select(
                Select(
                  Ident(newFreeTerm("__this", symbolForName("scala.reflect.DynamicProxy").asType, this))
                  , newTermName("dynamicProxyTarget")
                ),
                newTermName("asInstanceOf") )
              , List(TypeTree().setType(classToType(class_)))
            )
            ,newTermName(method).encodedName
          )
          ,args.map{ case(name,box) =>
            val value = Ident(newFreeTerm("__arg"+({i+=1;i}.toString), classToType(box.class_), box.value))
            if( name == "" ) value
            else AssignOrNamedArg( Ident(name), value )
          }.toList
        )
      toolbox.runExpr( call )
    }
  }
}
