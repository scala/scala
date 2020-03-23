import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.language.postfixOps
import scala.annotation.StaticAnnotation

object kaseMacro {
  // ======= DIFFERENCES WITH VANILLA CASE =======
  // (this list is probably not exhaustive, since I didn't have time to study all details of the existing implementation
  // 1) No CASE and SYNTHETIC flags (those have special treatment in the compiler, which I didn't want to interact with)
  // 2) No warnings specially tailored for case classes, because the compiler knows how methods like equal will behave
  // 3) No error messages specially tailored for case classes in case of synthesis conflicts (e.g. implicit case class)
  // 4) No special treatment for case class pattern matching (constructor patterns, refutability checks)
  // 5) No special treatment in RefChecks.relativeVariance (I don't even know why that one's necessary)
  // 6) No specialized hashcode implementation for primitive fields (I didn't have time to go into all the details of codegen)
  // 7) No referential transparency for methods like apply or copy that are injected into the companion, but should use kase class definition scope

  abstract class kaseHelper[C <: Context](val c: C) {
    import c.universe._
    import c.universe.{Flag => PublicFlags}
    import scala.reflect.internal.{Flags => InternalFlags}

    def isTrait(mods: Modifiers) = (mods.flags.asInstanceOf[Long] & InternalFlags.TRAIT) != 0
    def isFinal(mods: Modifiers) = (mods.flags.asInstanceOf[Long] & InternalFlags.FINAL) != 0
    def isByName(mods: Modifiers) = (mods.flags.asInstanceOf[Long] & InternalFlags.BYNAMEPARAM) != 0
    def isAbstract(mods: Modifiers) = (mods.flags.asInstanceOf[Long] & InternalFlags.ABSTRACT) != 0
    val ParamMods = Modifiers(InternalFlags.PARAM.asInstanceOf[Long].asInstanceOf[FlagSet])
    val SyntheticMods = Modifiers((0 /* | InternalFlags.SYNTHETIC */).asInstanceOf[Long].asInstanceOf[FlagSet])
    val SyntheticCaseMods = Modifiers((0 /* | InternalFlags.SYNTHETIC | InternalFlags.CASE */).asInstanceOf[Long].asInstanceOf[FlagSet])
    val OverrideSyntheticMods = Modifiers(PublicFlags.OVERRIDE /* | InternalFlags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet] */)
    val FinalOverrideSyntheticMods = Modifiers(PublicFlags.FINAL | PublicFlags.OVERRIDE /* | InternalFlags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet] */)
    def makeCase(mods: Modifiers) = {
      val flags1 = mods.flags.asInstanceOf[Long] /* | InternalFlags.CASEACCESSOR */
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }
    def makeCaseAccessor(mods: Modifiers) = {
      if (isByName(mods)) c.abort(c.enclosingPosition, "`kase` parameters may not be call-by-name")
      val flags1 = mods.flags.asInstanceOf[Long] /* | InternalFlags.CASEACCESSOR */ & ~InternalFlags.PRIVATE & ~InternalFlags.LOCAL
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }
    def unmakeDefault(mods: Modifiers) = {
      var flags1 = mods.flags.asInstanceOf[Long] & ~InternalFlags.DEFAULTPARAM
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }
    def unmakeCaseAccessor(mods: Modifiers) = {
      var flags1 = mods.flags.asInstanceOf[Long] & ~InternalFlags.PARAMACCESSOR & ~InternalFlags.CASEACCESSOR
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }
    def makeDefault(mods: Modifiers) = Modifiers(mods.flags | PublicFlags.DEFAULTPARAM, mods.privateWithin, mods.annotations)
    def unmakeParam(mods: Modifiers) = {
      var flags1 = mods.flags.asInstanceOf[Long] & ~InternalFlags.PARAM
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }
    def makeDeferredSynthetic(mods: Modifiers) = {
      var flags1 = mods.flags.asInstanceOf[Long] | InternalFlags.DEFERRED /* | InternalFlags.SYNTHETIC */
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }
    def unmakeVariant(mods: Modifiers) = {
      var flags1 = mods.flags.asInstanceOf[Long] & ~InternalFlags.COVARIANT & ~InternalFlags.CONTRAVARIANT
      Modifiers(flags1.asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
    }

    def expand(annottees: List[c.Tree]): List[c.Tree]
  }

  class kaseClassHelper[C <: Context](override val c: C) extends kaseHelper(c) {
    import c.universe._
    import definitions._

    def expand(annottees: List[c.Tree]): List[c.Tree] = {
      val cdef @ ClassDef(_, name, tparams, Template(_, _, cbody)) = annottees.head
      val primaryCtor = cbody.collect{ case ddef @ DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => ddef }.head
      if (primaryCtor.vparamss.isEmpty) c.abort(c.enclosingPosition, "`kase' is not applicable to classes without a parameter list")
      val primaryParamss = primaryCtor.vparamss
      val primaryParams = primaryParamss.head
      val secondaryParamss = primaryParamss.tail
      val ourPolyType = if (tparams.nonEmpty) AppliedTypeTree(Ident(name), tparams.map(tparam => Ident(tparam.name))) else Ident(name)
      val tparamUnderscores = tparams.zipWithIndex.map{ case (tdef, i) => TypeDef(makeDeferredSynthetic(unmakeParam(tdef.mods)), TypeName("x$" + (i+1)), tdef.tparams, tdef.rhs) }
      val ourExistentialType = ExistentialTypeTree(AppliedTypeTree(Ident(name), tparamUnderscores.map(tdef => Ident(tdef.name))), tparamUnderscores)

      val kaseClass = {
        val ClassDef(cmods, _, _, Template(cparents, cself, _)) = cdef

        // step 1: make it a case class
        val cmods1 = makeCase(cmods)

        // step 2: turn param accessors into case accessors
        val cbody2 = cbody.map {
          case ValDef(mods, name, tpt, rhs) if primaryParams.exists(_.name == name) => ValDef(makeCaseAccessor(mods), name, tpt, rhs)
          case stat => stat
        }

        // step 3: inject copy if not defined
        val cbody3 = {
          if (cbody2.collect{ case ddef @ DefDef(_, name, _, _, _, _) if name == TermName("copy") => ddef }.nonEmpty) cbody2
          else {
            val copyTparams = tparams
            val primaryCopyParamss = primaryParams.map(p => ValDef(makeDefault(unmakeCaseAccessor(p.mods)), p.name, p.tpt, Ident(p.name)))
            val secondaryCopyParamss = secondaryParamss.map(_.map(p => ValDef(unmakeDefault(unmakeCaseAccessor(p.mods)), p.name, p.tpt, EmptyTree)))
            val copyParamss = primaryCopyParamss :: secondaryCopyParamss
            val copyArgss = copyParamss.map(_.map(p => Ident(p.name)))
            val copyBody = copyArgss.foldLeft(Select(New(Ident(cdef.name)), termNames.CONSTRUCTOR): Tree)((callee, args) => Apply(callee, args))
            val copyMethod = DefDef(SyntheticMods, TermName("copy"), copyTparams, copyParamss, TypeTree(), copyBody)
            cbody2 :+ copyMethod
          }
        }

        // step 4: implement product
        val cparents4 = cparents :+ Select(Ident(TermName("scala")), TypeName("Product")) :+ Select(Ident(TermName("scala")), TypeName("Serializable"))
        val cbody4 = {
          val productPrefixMethod = DefDef(OverrideSyntheticMods, TermName("productPrefix"), Nil, Nil, TypeTree(), Literal(Constant(name.toString)))
          val productArityMethod = DefDef(SyntheticMods, TermName("productArity"), Nil, Nil, TypeTree(), Literal(Constant(primaryParams.length)))
          val productElementParam = ValDef(ParamMods, TermName("x$1"), Select(Ident(TermName("scala")), TypeName("Int")), EmptyTree)
          def productElementByIndex(i: Int) = CaseDef(Literal(Constant(i)), EmptyTree, Select(This(name), primaryParams(i).name))
          val productElementFallback = CaseDef(Ident(termNames.WILDCARD), EmptyTree, Throw(Apply(Select(New(TypeTree(c.mirror.staticClass("java.lang.IndexOutOfBoundsException").toType)), termNames.CONSTRUCTOR), List(Apply(Select(Ident(productElementParam.name), TermName("toString")), List())))))
          val productElementBody = Match(Ident(productElementParam.name), (0 until primaryParams.length map productElementByIndex toList) :+ productElementFallback)
          val productElementMethod = DefDef(SyntheticMods, TermName("productElement"), Nil, List(List(productElementParam )), TypeTree(), productElementBody)
          val scalaRunTime = Select(Select(Ident(TermName("scala")), TermName("runtime")), TermName("ScalaRunTime"))
          val productIteratorBody = Apply(TypeApply(Select(scalaRunTime, TermName("typedProductIterator")), List(Ident(TypeName("Any")))), List(This(name)))
          val productIteratorMethod = DefDef(OverrideSyntheticMods, TermName("productIterator"), Nil, Nil, TypeTree(), productIteratorBody)
          val canEqualParam = ValDef(ParamMods, TermName("x$1"), Select(Ident(TermName("scala")), TypeName("Any")), EmptyTree)
          val canEqualBody = TypeApply(Select(Ident(canEqualParam.name), TermName("isInstanceOf")), List(ourExistentialType))
          val canEqualMethod = DefDef(SyntheticMods, TermName("canEqual"), Nil, List(List(canEqualParam)), TypeTree(), canEqualBody)
          cbody3 ++ List(productPrefixMethod, productArityMethod, productElementMethod, productIteratorMethod, canEqualMethod)
        }

        // step 5: inject hashcode
        val cbody5 = {
          val scalaRunTime = Select(Select(Ident(TermName("scala")), TermName("runtime")), TermName("ScalaRunTime"))
          val hashcodeMethod = DefDef(OverrideSyntheticMods, TermName("hashCode"), Nil, Nil, TypeTree(), Apply(Select(scalaRunTime, TermName("_hashCode")), List(This(name))))
          cbody4 :+ hashcodeMethod
        }

        // step 6: inject meaningful toString if not defined
        val cbody6 = {
          if (cbody5.collect{ case ddef @ DefDef(_, name, _, _, _, _) if name == TermName("toString") => ddef }.nonEmpty) cbody5
          else {
            val scalaRunTime = Select(Select(Ident(TermName("scala")), TermName("runtime")), TermName("ScalaRunTime"))
            val toStringBody = Apply(Select(scalaRunTime, TermName("_toString")), List(This(name)))
            val toStringMethod = DefDef(OverrideSyntheticMods, TermName("toString"), Nil, Nil, TypeTree(), toStringBody)
            cbody5 :+ toStringMethod
          }
        }

        // step 7: inject equals
        val cbody7 = {
          val equalsParam = ValDef(ParamMods, TermName("x$1"), Select(Ident(TermName("scala")), TypeName("Any")), EmptyTree)
          val equalsBody = {
            def thisEqThat = {
              val thatAnyRef = TypeApply(Select(Ident(equalsParam.name), TermName("asInstanceOf")), List(Ident(TypeName("Object"))))
              Apply(Select(This(name), TermName("eq")), List(thatAnyRef))
            }
            def thatCanEqualThis = {
              val thatC = TypeApply(Select(Ident(equalsParam.name), TermName("asInstanceOf")), List(ourPolyType))
              Apply(Select(thatC, TermName("canEqual")), List(This(name)))
            }
            def sameTypeCheck = {
              val ifSameType = CaseDef(Typed(Ident(termNames.WILDCARD), ourPolyType), EmptyTree, Literal(Constant(true)))
              val otherwise = CaseDef(Ident(termNames.WILDCARD), EmptyTree, Literal(Constant(false)))
              Match(Ident(equalsParam.name), List(ifSameType, otherwise))
            }
            def sameFieldsCheck = {
              val thatC = ValDef(SyntheticMods, TermName(name.toString + "$1"), ourPolyType, TypeApply(Select(Ident(equalsParam.name), TermName("asInstanceOf")), List(ourPolyType)))
              val sameFieldsChecks = primaryParams.map(p => Apply(Select(Select(This(name), p.name), TermName("==").encodedName), List(Select(Ident(thatC.name), p.name))))
              val thatCanEqualThis = Apply(Select(Ident(thatC.name), TermName("canEqual")), List(This(name)))
              val sameFieldCheck = (sameFieldsChecks :+ thatCanEqualThis).reduceLeft((acc, check) => Apply(Select(acc, TermName("&&").encodedName), List(check)))
              Block(List(thatC), sameFieldCheck)
            }
            if (primaryParamss.isEmpty) {
              if (isFinal(cmods)) sameTypeCheck
              else Apply(Select(sameTypeCheck, TermName("&&").encodedName), List(thatCanEqualThis))
            } else {
              val thisEqualsThat = Apply(Select(sameTypeCheck, TermName("&&").encodedName), List(sameFieldsCheck))
              Apply(Select(thisEqThat, TermName("||").encodedName), List(thisEqualsThat))
            }
          }
          val equalsMethod = DefDef(OverrideSyntheticMods, TermName("equals"), Nil, List(List(equalsParam)), TypeTree(), equalsBody)
          cbody6 :+ equalsMethod
        }

        ClassDef(cmods1, name, tparams, Template(cparents4, cself, cbody7))
      }

      val kaseModule = {
        val mdef @ ModuleDef(mmods, mname, Template(mparents, mself, mbody)) = annottees.tail.headOption getOrElse {
          val shouldInheritFromFun = !isAbstract(cdef.mods) && tparams.isEmpty && primaryParamss.length == 1
          val funClass = Select(Select(Ident(TermName("scala")), TermName("runtime")), TypeName("AbstractFunction" + primaryParams.length))
          val funParent = AppliedTypeTree(funClass, primaryParams.map(_.tpt) :+ Ident(name))
          val parents = if (shouldInheritFromFun) List(funParent) else List(Ident(AnyRefClass))
          val emptyCtor = DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(()))))
          ModuleDef(SyntheticMods, name.toTermName, Template(parents, noSelfType, List(emptyCtor)))
        }

        // step 1: inject toString if not defined
        val mbody1 = {
          if (mbody.collect{ case ddef @ DefDef(_, name, _, _, _, _) if name == TermName("toString") => ddef }.nonEmpty) mbody
          else {
            val toStringBody = Literal(Constant(name.toString))
            val toStringMethod = DefDef(FinalOverrideSyntheticMods, TermName("toString"), Nil, Nil, TypeTree(), toStringBody)
            mbody :+ toStringMethod
          }
        }

        // step 2: inject apply
        val mbody2 = {
          val applyTparams = tparams.map(p => TypeDef(unmakeVariant(p.mods), p.name, p.tparams, p.rhs))
          val applyParamss = primaryParamss.map(_.map(p => ValDef(unmakeCaseAccessor(p.mods), p.name, p.tpt, p.rhs)))
          val applyArgss = applyParamss.map(_.map(p => Ident(p.name)))
          val applyBody = applyArgss.foldLeft(Select(New(ourPolyType), termNames.CONSTRUCTOR): Tree)((callee, args) => Apply(callee, args))
          val applyMethod = DefDef(SyntheticCaseMods, TermName("apply"), applyTparams, applyParamss, TypeTree(), applyBody)
          mbody1 :+ applyMethod
        }

        // step 3: inject unapply
        val mbody3 = {
          val unapplyTparams = tparams.map(p => TypeDef(unmakeVariant(p.mods), p.name, p.tparams, p.rhs))
          val unapplyParam = ValDef(ParamMods, TermName("x$0"), ourPolyType, EmptyTree)
          val unapplyName = primaryParams match {
            case _ :+ AppliedTypeTree(tpt: RefTree, _) if tpt.name == TypeName("<repeated>") => TermName("unapplySeq")
            case _ => TermName("unapply")
          }
          val unapplyBody = {
            val none = Select(Ident(TermName("scala")), TermName("None"))
            def some(xs: Tree*) = Apply(Select(Ident(TermName("scala")), TermName("Some")), xs.toList)
            val thisEqNull = Apply(Select(Ident(unapplyParam.name), TermName("==").encodedName), List(Literal(Constant(null))))
            val failure = primaryParams match {
              case Nil => Literal(Constant(false))
              case _ => none
            }
            val success = primaryParams match {
              case Nil => Literal(Constant(true))
              case ps =>
                val fs = ps.map(p => Select(Ident(unapplyParam.name), p.name))
                val tuple = if (ps.length == 1) fs.head else Apply(Select(Ident(TermName("scala")), TermName("Tuple" + ps.length)), fs)
                some(tuple)
            }
            If(thisEqNull, failure, success)
          }
          val unapplyMethod = DefDef(SyntheticCaseMods, unapplyName, unapplyTparams, List(List(unapplyParam)), TypeTree(), unapplyBody)
          mbody2 :+ unapplyMethod
        }

        ModuleDef(mmods, mname, Template(mparents, mself, mbody3))
      }

      List(kaseClass, kaseModule)
    }
  }

  class kaseObjectHelper[C <: Context](override val c: C) extends kaseHelper(c) {
    import c.universe._

    def expand(annottees: List[c.Tree]): List[c.Tree] = {
      val mdef @ ModuleDef(mods, name, Template(parents, self, body)) :: Nil = annottees

      // step 1: make it a case object
      val mods1 = makeCase(mods)

      // step 2: implement product
      val parents2 = parents :+ Select(Ident(TermName("scala")), TypeName("Product")) :+ Select(Ident(TermName("scala")), TypeName("Serializable"))
      val body2 = {
        val productPrefixMethod = DefDef(OverrideSyntheticMods, TermName("productPrefix"), Nil, Nil, TypeTree(), Literal(Constant(name.toString)))
        val productArityMethod = DefDef(SyntheticMods, TermName("productArity"), Nil, Nil, TypeTree(), Literal(Constant(0)))
        val productElementParam = ValDef(ParamMods, TermName("x$1"), Select(Ident(TermName("scala")), TypeName("Int")), EmptyTree)
        val productElementBody = Match(Ident(productElementParam.name), List(CaseDef(Ident(termNames.WILDCARD), EmptyTree, Throw(Apply(Select(New(TypeTree(c.mirror.staticClass("java.lang.IndexOutOfBoundsException").toType)), termNames.CONSTRUCTOR), List(Apply(Select(Ident(productElementParam.name), TermName("toString")), List())))))))
        val productElementMethod = DefDef(SyntheticMods, TermName("productElement"), Nil, List(List(productElementParam )), TypeTree(), productElementBody)
        val scalaRunTime = Select(Select(Ident(TermName("scala")), TermName("runtime")), TermName("ScalaRunTime"))
        val productIteratorBody = Apply(TypeApply(Select(scalaRunTime, TermName("typedProductIterator")), List(Ident(TypeName("Any")))), List(This(name.toTypeName)))
        val productIteratorMethod = DefDef(OverrideSyntheticMods, TermName("productIterator"), Nil, Nil, TypeTree(), productIteratorBody)
        val canEqualParam = ValDef(ParamMods, TermName("x$1"), Select(Ident(TermName("scala")), TypeName("Any")), EmptyTree)
        val canEqualBody = TypeApply(Select(Ident(canEqualParam.name), TermName("isInstanceOf")), List(SingletonTypeTree(Ident(name))))
        val canEqualMethod = DefDef(SyntheticMods, TermName("canEqual"), Nil, List(List(canEqualParam)), TypeTree(), canEqualBody)
        body ++ List(productPrefixMethod, productArityMethod, productElementMethod, productIteratorMethod, canEqualMethod)
      }

      // step 3: inject hashcode
      val body3 = {
        val hashcodeMethod = DefDef(OverrideSyntheticMods, TermName("hashCode"), Nil, Nil, TypeTree(), Literal(Constant((name.decodedName.toString.hashCode))))
        body2 :+ hashcodeMethod
      }

      // step 4: inject meaningful toString if not defined
      val body4 = {
        if (body3.collect{ case ddef @ DefDef(_, name, _, _, _, _) if name == TermName("toString") => ddef }.nonEmpty) body3
        else {
          val toStringMethod = DefDef(OverrideSyntheticMods, TermName("toString"), Nil, Nil, TypeTree(), Literal(Constant(name.toString)))
          body3 :+ toStringMethod
        }
      }

      List(ModuleDef(mods1, name, Template(parents2, self, body4)))
    }
  }

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val helper = {
      annottees.head.tree match {
        case ClassDef(_, _, _, _) => new kaseClassHelper[c.type](c)
        case ModuleDef(_, _, _) => new kaseObjectHelper[c.type](c)
        case _ => c.abort(c.enclosingPosition, "`kase` is only applicable to classes and objects")
      }
    }
    c.Expr[Any](Block(helper.expand(annottees.map(_.tree).toList), Literal(Constant(()))))
  }
}

class kase extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro kaseMacro.impl
}

package pkg {
  class kase extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro kaseMacro.impl
  }
}
