package scala.tools.nsc
package transform

import scala.tools.nsc.symtab.SymbolTable
import scala.reflect
import collection.mutable

/** Functions to reify (and un-reify) symbols, types, and trees.
 *  These can be used with only a symbol table; they do not
 *  need a full compiler.
 *
 *  @author Gilles Dubochet, Lex Spoon
 */
trait Reifiers {
  val symbols: Global
  import symbols._

  private def mkGlobalSymbol(fullname: String, sym: Symbol): reflect.Symbol =
    if (sym.isClass) reflect.Class(fullname)
    else if (sym.isType) reflect.TypeField(fullname, reify(sym.info))
    else if (sym.isMethod) reflect.Method(fullname, reify(sym.info))
    else if (sym.isValueParameter) reflect.LocalValue(reflect.NoSymbol, fullname, reify(sym.info))
    else reflect.Field(fullname, reify(sym.info));

  def reify(sym: Symbol): reflect.Symbol = {
    if (sym.isRoot || sym.isRootPackage || sym.isEmptyPackageClass || sym.isEmptyPackage)
      reflect.RootSymbol
    else if (sym.isValueParameter)
      mkGlobalSymbol(sym.name.toString, sym)
    else if (sym.owner.isTerm)
      reflect.NoSymbol
    else reify(sym.owner) match {
      case reflect.NoSymbol =>
        reflect.NoSymbol;
      case reflect.RootSymbol =>
        mkGlobalSymbol(sym.name.toString(), sym)
      case reflect.Class(ownername) =>
        mkGlobalSymbol(ownername + "." + sym.name, sym)
      case _ =>
        reflect.NoSymbol
    }
  }

  var _log_reify_type_ = false

  def reify(tp: Type): reflect.Type = tp match {
    case ErrorType =>
      reflect.NoType
    case WildcardType =>
      if (_log_reify_type_) println("cannot handle WildcardType")
    reflect.NoType
    case NoType =>
      reflect.NoType
    case NoPrefix =>
      reflect.NoType
    case ThisType(sym) =>
      val rsym = reify(sym)
      reflect.ThisType(rsym)
    case SingleType(pre, sym) =>
      reflect.SingleType(reify(pre), reify(sym))
    case ConstantType(value) =>
      reify(value.tpe)
    case TypeRef(pre, sym, args) =>
      val rpre = reify(pre)
      val rsym = reify(sym)
      val rargs = args map reify
      val beforeArgs = reflect.PrefixedType(rpre, rsym)
      if (rargs.isEmpty)
	beforeArgs
      else if (rpre == reflect.NoType || rsym == reflect.NoSymbol)
	beforeArgs
      else
	reflect.AppliedType(beforeArgs, rargs)
    case TypeBounds(lo, hi) =>
      reflect.TypeBounds(reify(lo), reify(hi))
    case RefinedType(parents, defs) =>
      if (_log_reify_type_) println("cannot handle RefinedType "+tp); reflect.NoType
    case ClassInfoType(parents, defs, clazz) =>
      if (_log_reify_type_) println("cannot handle ClassInfoType "+tp); reflect.NoType
    case MethodType(params, result) =>
      reflect.MethodType(params.map(reify), reify(result))
    case NullaryMethodType(result) =>
      reflect.NullaryMethodType(reify(result))
    case PolyType(tparams, result) =>
      val boundss =
	for {
	  param <- tparams
	  TypeBounds(lo,hi) = param.info.bounds
	} yield (reify(lo), reify(hi))

      reflect.PolyType(
	tparams.map(reify),
	boundss,
	reify(result))
      //todo: treat ExistentialType
    case AnnotatedType(annots, tp, _) =>
      reify(tp)
    case _ =>
      println("could not reify: " + tp)
      reflect.NoType
  }


  /** This is woefully incomplete.  It is barely enough
   *  to process the types of Constant's .
   */
  def unreify(tpe: reflect.Type): Type =
    tpe match {
      case reflect.NoPrefix => NoPrefix
      case reflect.NoType => NoType
      case reflect.NamedType(fullname) =>
	//NamedType(fullname)
	println("NamedType: " + fullname)
	NoType
      case reflect.PrefixedType(_, reflect.Class("scala.Array")) =>
        definitions.ArrayClass.tpe
      case reflect.PrefixedType(_, reflect.Class("java.lang.String")) =>
        definitions.StringClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Unit")) =>
        definitions.UnitClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Boolean")) =>
        definitions.BooleanClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Byte")) =>
        definitions.ByteClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Short")) =>
        definitions.ShortClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Int")) =>
        definitions.IntClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Long")) =>
        definitions.LongClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Float")) =>
        definitions.FloatClass.tpe
      case reflect.PrefixedType(_, reflect.Class("scala.Double")) =>
        definitions.DoubleClass.tpe
      case reflect.PrefixedType(pre, sym) =>
	NoType
      case reflect.SingleType(pre, sym) =>
	SingleType(unreify(pre), unreify(sym))
      case reflect.ThisType(clazz) =>
	ThisType(unreify(clazz))
      case reflect.AppliedType(tpe, args) =>
	val untpe = unreify(tpe)
	if (untpe == NoType)
	  NoType
	else
	  appliedType(untpe, args.map(unreify))
      case reflect.TypeBounds(lo, hi) =>
	TypeBounds(unreify(lo), unreify(hi))
      case reflect.MethodType(params, restpe) =>
	MethodType(params.map(unreify), unreify(restpe))
      case reflect.NullaryMethodType(restpe) =>
	NullaryMethodType(unreify(restpe))
      case reflect.PolyType(typeParams, typeBounds, resultType) =>
	PolyType(typeParams.map(unreify), unreify(resultType))
      //todo: treat ExistentialType
      case _ => NoType
    }


  /** This is woefully incomplete.  It is barely enough
   *  to process the types of Constant's .
   */
  def unreify(symbol: reflect.Symbol): Symbol =
    symbol match {
      case reflect.Class(fullname) =>
	fullname match {
	  case "scala.Unit" => definitions.UnitClass
	  case "scala.Boolean" => definitions.BooleanClass
	  case "scala.Byte" => definitions.ByteClass
	  case "scala.Short" => definitions.ShortClass
	  case "scala.Int" => definitions.IntClass
	  case "scala.Long" => definitions.LongClass
	  case "scala.Float" => definitions.FloatClass
	  case "scala.Double" => definitions.DoubleClass

	  case "scala.Array" => definitions.ArrayClass

	  case _ => NoSymbol

	}

      case _ => NoSymbol
    }

  case class FreeValue(tree: Tree) extends reflect.Tree

  class ReifyEnvironment extends mutable.HashMap[Symbol, reflect.Symbol] {
    var targets = new mutable.HashMap[String, Option[reflect.LabelSymbol]]()
    def addTarget(name: String, target: reflect.LabelSymbol): Unit =
      targets.update(name, Some(target))
    def getTarget(name: String): Option[reflect.LabelSymbol] =
      targets.get(name) match {
        case None =>
          targets.update(name, None)
          None
        //case Some(None) => None //bq:redundant
        case Some(tgt) => tgt
      }
    def hasAllTargets: Boolean =
      targets.iterator.map(_._2).forall {
        case Some(_) => true
        case None => false
      }
    override def update(sym: Symbol, rsym: reflect.Symbol) =
      super.update(sym,rsym)
  }


  class Reifier(env: ReifyEnvironment, currentOwner: reflect.Symbol)
  {
    def reify(tree: Tree): reflect.Tree = tree match {
      case Ident(_) =>
        val rsym = reify(tree.symbol);
        //Console.println("LiftCode: seen ident")
        if (rsym == reflect.NoSymbol) {
          //Console.println("  free = "+tree)
          FreeValue(tree)
        } else {
          //Console.println("  rsym = "+rsym)
          reflect.Ident(rsym)
        }
      case Select(qual, _) =>
        val rsym = reify(tree.symbol);
        if (rsym == reflect.NoSymbol) throw new TypeError("cannot reify symbol: " + tree.symbol)
        else reflect.Select(reify(qual), reify(tree.symbol))

      case Literal(constant) =>
        reflect.Literal(constant.value)

      case Apply(name, args) if name.toString().startsWith("label$") =>
        env.getTarget(name.toString()) match {
          case None => throw new TypeError("cannot reify tree (no forward jumps allowed): " + tree)
          case Some(label) => reflect.Goto(label)
        }

      case Apply(fun, args) =>
        reflect.Apply(reify(fun), args map reify)

      case TypeApply(fun, args) =>
        reflect.TypeApply(reify(fun), args map (_.tpe) map reify)

      case Function(vparams, body) =>
        var env1 = env
        for (vparam <- vparams) {
          val local = reflect.LocalValue(
            currentOwner, vparam.symbol.name.toString(), reify(vparam.symbol.tpe));
          env1.update(vparam.symbol, local);
        }
        reflect.Function(vparams map (_.symbol) map env1,
                         new Reifier(env1, currentOwner).reify(body))
      case tree@This(_) if tree.symbol.isModule =>
        // there is no reflect node for a module's this, so
        // represent it as a selection of the module
	reify(
	  Select(This(tree.symbol.owner), tree.symbol.name))
      case This(_) =>
        reflect.This(reify(tree.symbol))
      case Block(stats, expr) =>
        reflect.Block(stats.map(reify), reify(expr))
      case New(clazz) if (clazz.isType) =>
	val reifiedSymbol = reify(clazz.symbol)
	reflect.New(reflect.Ident(reifiedSymbol))
      case New(clazz) =>
        val reifiedClass = reify(clazz)
        reflect.New(reifiedClass)
      case Typed(t, _) =>
        reify(t)
      case If(cond, thenp, elsep) =>
        reflect.If(reify(cond), reify(thenp), reify(elsep))
      case Assign(lhs, rhs) =>
        reflect.Assign(reify(lhs), reify(rhs))

      case LabelDef(name, Nil, body) =>
        val sym = new reflect.LabelSymbol(name.toString())
        env.addTarget(name.toString(), sym)
        val res = reflect.Target(sym, reify(body))
        res

      case vd @ ValDef(mods, name, tpt, rhs) =>
        val rtpe = reify(vd.tpe) // will return null, currently?!
        val sym  = reflect.LocalValue(currentOwner, name.toString(), rtpe)
        env(vd.symbol) = sym // bq: despite Scala's scoping rules, this should work because references to vd.symbol were type checked.
        val rhs_ = reify(rhs)
        reflect.ValDef(sym, rhs_)

      case cd @ ClassDef(mods, name, tparams, impl) =>
        if(!tparams.isEmpty)
          throw new TypeError("cannot handle polymorphic ClassDef ("+name+"): " + tparams)
        val rsym = reify(cd.symbol)
        val rimp = reify(impl)
        val rtpe = reify(impl.self.tpt.tpe) //todo: update
        reflect.ClassDef(rsym, rtpe, rimp.asInstanceOf[reflect.Template])

      case tmpl @ Template(parents, self, body) =>
        val rparents = for (p <- parents) yield { reify(p.tpe) }
        //todo: add self to reified templates
        reflect.Template(rparents, body.map(reify))

      case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        if(!tparams.isEmpty)
          throw new TypeError("cannot handle polymorphic DefDef ("+name+"): " + tparams)
        val rsym   = reify(dd.symbol)
        val rparss = vparamss map { x => x map (reify) }
        val rret   = reify(tpt.tpe)
        val rrhs   = reify(rhs)
        reflect.DefDef(rsym, rparss, rret, rrhs)

      case sp @ Super(qual, mix) =>
        val rsym = reify(sp.symbol)
        reflect.Super(rsym)

      case _ =>
        throw new TypeError("cannot reify tree ("+tree.getClass()+"): " + tree)
    }

    def reify(sym: Symbol): reflect.Symbol =
      env.get(sym) match {
	case Some(rsym) =>
	  rsym
	case None =>
	  Reifiers.this.reify(sym)
      }

    def reify(tpe: Type): reflect.Type =
      Reifiers.this.reify(tpe)
  }

  def reify(tree: Tree): reflect.Tree =
    new Reifier(new ReifyEnvironment(), reflect.NoSymbol).reify(tree)
}
