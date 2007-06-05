package scala.tools.nsc.transform
import scala.tools.nsc.symtab.SymbolTable
import scala.reflect

/** Functions to reify (and un-reify) symbols and types.
 *  These can be used with only a symbol table; they do not
 *  need a full compiler.
 *
 *  @author Gilles Dubochet, Lex Spoon
 */
trait SymbolReifier {
  val symbols: SymbolTable
  import symbols._

  private def mkGlobalSymbol(fullname: String, sym: Symbol): reflect.Symbol =
    if (sym.isClass) reflect.Class(fullname)
    else if (sym.isType) reflect.TypeField(fullname, reify(sym.info))
    else if (sym.isMethod) reflect.Method(fullname, reify(sym.info))
    else reflect.Field(fullname, reify(sym.info));

  def reify(sym: Symbol): reflect.Symbol = {
    if (sym.isRoot || sym.isRootPackage || sym.isEmptyPackageClass || sym.isEmptyPackage)
      reflect.RootSymbol
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
      if(rargs.isEmpty)
	beforeArgs
      else if(beforeArgs == NoType)
	beforeArgs
      else
	reflect.AppliedType(beforeArgs, rargs)
    case TypeBounds(lo, hi) =>
      reflect.TypeBounds(reify(lo), reify(hi))
    case RefinedType(parents, defs) =>
      if (_log_reify_type_) println("cannot handle RefinedType "+tp); reflect.NoType
    case ClassInfoType(parents, defs, clazz) =>
      if (_log_reify_type_) println("cannot handle ClassInfoType "+tp); reflect.NoType
    case MethodType(paramtypes, result) =>
      reflect.MethodType(paramtypes.map(reify), reify(result))
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
    case AnnotatedType(attribs, tp) =>
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
      case reflect.MethodType(formals, restpe) =>
	MethodType(formals.map(unreify), unreify(restpe))
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
}
