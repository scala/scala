import collection.immutable.ArraySeq

object Test {

  def main(args: Array[String]): Unit = {

    val BarTypeConstructor = // [B <: CC[Int], CC[_]] => Bar[B, CC]
      PolyType.from(
        params = List(
          "B" -> (hk => TypeBounds.upper(AppliedType(hk.ref(1), IntType :: Nil))),
          "CC" -> (hk => PolyType.from(List("_" -> (_ => TypeBounds.upper(AnyType))), hk => AnyType))
        ),
        res = hk => AppliedType(NamedRef("Bar"), hk.ref(0) :: hk.ref(1) :: Nil)
      )

    println(BarTypeConstructor.debug)
    println()

    try {
      val DeltaTypeConstructor = // [B <: CC[[I <: B] =>> Any], CC[_[_ <: B]]] =>> Delta[B, CC]
        PolyType.from(
          params = List(
            "B" -> (hk =>
              TypeBounds.upper(
                AppliedType(
                  tycon = hk.ref(1),
                  args = PolyType.from(List("I" -> (_ => TypeBounds.upper(hk.ref(0)))), _ => AnyType) :: Nil
                )
              )
            ),
            "CC" -> (hk =>
              PolyType.from(
                params = List(
                  "_" -> (_ =>
                    PolyType.from(
                      params = List(
                        "_" -> (_ =>
                          // force a cyclic completion - this type is illegal in Dotty
                          // a completion would be needed here to check the bounds of `CC`
                          TypeBounds.upper({val ref = hk.ref(0); ref.underlying; ref})
                        )
                      ),
                      res = hk => AnyType
                    )
                  )
                ),
                res = hk => AnyType
              )
            )
          ),
          res = hk => AppliedType(NamedRef("Delta"), hk.ref(0) :: hk.ref(1) :: Nil)
        )
    } catch {
      case err: AssertionError =>
        assert(err.getMessage.contains("cyclic completion of SyncRef"))
        println("there was a cycle in creating Delta type constructor")
    }
  }
}

final class SyncRef[A](private var compute: () => A) {
  private var out: A = _
  private var entered: Boolean = false

  def apply(): A = {
    if (entered) {
      assert(out != null, "cyclic completion of SyncRef")
    }
    else {
      entered = true
      val result = compute()
      compute = null
      assert(result != null, "SyncRef is non-nullable")
      out = result
    }
    out
  }
}

sealed abstract class TypeOrCompleter {
  def debug: String = this match {
    case p: Product => s"${p.productPrefix}${
      def iter(it: Iterator[Any], s: String = "(", e: String = ")"): String =
        it.map {
          case t: Type => t.debug
          case t: Iterable[u] => iter(t.iterator, s = "[", e = "]")
          case a => a.toString
        }.mkString(s, ", ", e)
      val it = p.productIterator
      if (!it.hasNext) "" else iter(it)
    }"
    case _ => toString
  }
}

abstract class Completer extends TypeOrCompleter {
  def complete(sym: Symbol): Unit
}

abstract class Type extends TypeOrCompleter {
  def underlying: Type = this
}

class Symbol(val name: String, private var myInfoOrCompleter: TypeOrCompleter) { self =>

  def infoOrCompleter = myInfoOrCompleter

  def info_=(tp: Type): Unit =
    myInfoOrCompleter = tp

  def info: Type = myInfoOrCompleter match {
    case c: Completer =>
      c.complete(self)
      info
    case t: Type => t
  }

  override def toString = s"$name => ${infoOrCompleter.debug}"

}

case class ParamRef(symbol: Symbol) extends Type {
  override def underlying: Type = symbol.info
  override def debug: String = s"ParamRef(${symbol.name})"
}

case class PolyType(params: List[Symbol], resultType: Type) extends Type
case class AppliedType(tycon: Type, args: List[Type]) extends Type
case class TypeBounds(lo: Type, hi: Type) extends Type
object TypeBounds {
  def upper(hi: Type) = TypeBounds(NothingType, hi)
}
case object IntType extends Type
case object AnyType extends Type
case object NothingType extends Type
case class NamedRef(fullname: String) extends Type

object PolyType {
  def from(params: List[(String, HKTypeLambda => Type)], res: HKTypeLambda => Type): PolyType = {
    val (names, infos0) = params.to(ArraySeq).unzip
    val infos = (hk: HKTypeLambda) => () => infos0.map { case op => op(hk) }
    new HKTypeLambda(names, infos, res).underlying
  }
}

class HKTypeLambda(paramNames: ArraySeq[String], paramInfosOp: HKTypeLambda => () => ArraySeq[Type], resOp: HKTypeLambda => Type) { thisLambda =>

  final val lambdaParams = {
    val paramInfoDb = new SyncRef(paramInfosOp(thisLambda))
    paramNames.zipWithIndex.map { case (name, idx) =>
      new Symbol(name, new Completer {
        def complete(sym: Symbol): Unit = {
          sym.info = paramInfoDb()(idx)
        }
      })
    }
  }

  final val resType = resOp(thisLambda)

  def ref(idx: Int): ParamRef = new ParamRef(lambdaParams(idx))

  def underlying: PolyType = {
    lambdaParams.foreach(_.info)
    new PolyType(lambdaParams.toList, resType)
  }

}
