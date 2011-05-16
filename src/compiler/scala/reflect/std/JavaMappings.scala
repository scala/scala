package scala.reflect
package std

import common._
import collection.mutable.ListBuffer

trait JavaMappings { self: Mirror =>

  import definitions._

  type TypeParamMap = Map[java.lang.reflect.TypeVariable[_], Symbol]

  private val localDummy = definitions.RootClass.newValue(NoPosition, "<local-dummy>").setFlag(Flags.SYNTHETIC).setInfo(NoType)

  def fromJava(clazz: java.lang.Class[_]): Symbol = clazz match {
    case java.lang.Boolean.TYPE => BooleanClass
    case java.lang.Character.TYPE => CharClass
    case java.lang.Byte.TYPE => ByteClass
    case java.lang.Short.TYPE => ShortClass
    case java.lang.Integer.TYPE => IntClass
    case java.lang.Long.TYPE => LongClass
    case java.lang.Float.TYPE => FloatClass
    case java.lang.Double.TYPE => DoubleClass
    case java.lang.Void.TYPE => UnitClass
    case _ => definitions.getClass(clazz.getName)
  }

  def fromJavaType(jtpe: java.lang.reflect.Type)(implicit tparMap: TypeParamMap): Type = jtpe match {
    case clazz: java.lang.Class[_] =>
      if (clazz.isArray) arrayType(fromJavaType(clazz.getComponentType))
      else {
        val sym = fromJava(clazz)
        if (clazz.isLocalClass) typeRef(NoPrefix, sym, List())
        else if (clazz.isMemberClass) typeRef(fromJavaType(clazz.getDeclaringClass), sym, List())
        else sym.tpe
      }
    case ptpe: java.lang.reflect.ParameterizedType =>
      val TypeRef(pre0, sym, _) = fromJavaType(ptpe.getRawType)
      val jpre = ptpe.getOwnerType
      val pre = if (jpre != null) fromJavaType(jpre) else pre0
      val exbuf = new ListBuffer[Symbol]
      def fromJavaArgType(jtp: java.lang.reflect.Type) = fromJavaType(jtp) match {
        case BoundedWildcardType(bounds) =>
          val tparam = pre.typeSymbol.newTypeParameter(NoPosition, newTypeName("x$" + exbuf.length)).setInfo(bounds)
          exbuf += tparam
          tparam.tpe
        case tp => tp
      }
      val args = ptpe.getActualTypeArguments.toList map fromJavaArgType
      existentialAbstraction(exbuf.toList, typeRef(pre, sym, args))
    case gtpe: java.lang.reflect.GenericArrayType =>
      val etpe = fromJavaType(gtpe.getGenericComponentType)
      arrayType(etpe)
    case wtpe: java.lang.reflect.WildcardType =>
      BoundedWildcardType(
        TypeBounds(
          (wtpe.getLowerBounds map fromJavaType).headOption getOrElse NothingClass.tpe,
          intersectionType((wtpe.getUpperBounds map fromJavaType).toList)))
    case vtpe: java.lang.reflect.TypeVariable[_] =>
      tparMap(vtpe).tpe
  }
}