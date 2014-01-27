import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object Test extends App {
  var typeMembers = typeOf[scala.reflect.api.Universe].members.filter(sym => sym.isType && !sym.isClass).toList
  typeMembers = typeMembers.filter(_.name != TypeName("ModifiersCreator")) // type ModifiersCreator = ModifiersExtractor
  val tags = typeOf[scala.reflect.api.Universe].members.filter(sym => sym.isImplicit).toList

  typeMembers.foreach(_.typeSignature)
  tags.foreach(_.typeSignature)

  val outliers = typeMembers.filter(tm => !tags.exists(tag => tag.typeSignature match {
    case NullaryMethodType(TypeRef(_, sym, targ :: Nil)) => sym == typeOf[ClassTag[_]].typeSymbol && targ.typeSymbol == tm
    case _ => false
  }))
  println(outliers)
}