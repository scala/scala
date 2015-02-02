trait Table[T]{
  def foo : T
}
trait Computer
trait Device
 
object schema{
  def lub[T]( a:T, b:T ) = ???
  lub(null:Computers,null:Devices)
}
trait Computers extends Table[Computer]{
  def foo : Computer
}
trait Devices extends Table[Device]{
  def foo : Device
  def foo : Device
}
/* Was:
Exception in thread "main" java.lang.AssertionError: assertion failed: List(method foo, method foo)
        at scala.Predef$.assert(Predef.scala:165)
        at scala.reflect.internal.Symbols$Symbol.suchThat(Symbols.scala:1916)
        at scala.reflect.internal.tpe.GlbLubs$$anonfun$23.apply(GlbLubs.scala:350)
        at scala.reflect.internal.tpe.GlbLubs$$anonfun$23.apply(GlbLubs.scala:349)
        at scala.collection.immutable.List.map(List.scala:272)
        at scala.reflect.internal.tpe.GlbLubs$class.lubsym$1(GlbLubs.scala:349)
*/