package scala.tools.nsc.symtab
import scala.tools.nsc.transform.SymbolReifier
import util._

/** AnnotationInfo and its helpers */
trait AnnotationInfos {
  self : SymbolTable =>


  /** Convert a reflect tree to a Constant, if possible */
  private def refltree2cons(tree: reflect.Tree): Option[Constant] =
    tree match {
      case reflect.Literal(v) => Some(Constant(v))

      case reflect.Apply(
        reflect.TypeApply(
          reflect.Select(_,
                reflect.Method(
		  "scala.Array.apply",
		  reflect.PolyType(_, _,
                    reflect.MethodType(_, reflect.AppliedType(arrayType,_))))),
		  List(elemType)),
        members) =>

	refltrees2consArray(
	  members,
	  reflect.AppliedType(arrayType, List(elemType)))


      case reflect.Apply(
        reflect.Select(_,
          reflect.Method(
            "scala.Array.apply",
            reflect.MethodType(_, arrayType))),
          members) =>

 	refltrees2consArray(members, arrayType)

      case tree =>
        //println("could not convert: " + tree);
        None
    }

  private object symbolReifier extends SymbolReifier {
    val symbols: AnnotationInfos.this.type = AnnotationInfos.this
  }

  /** Convert a sequence of trees to an array type,
   *  if all of the array elements are constants.
   *  Use arrayType as type of the resulting constant.
   */
  private def refltrees2consArray(
    trees: Seq[reflect.Tree],
    arrayType: reflect.Type)
  : Option[Constant] =
  {
        // println("arrayType is " + arrayType + " (" +
 	//         symbolReifier.unreify(arrayType) + ")")

    val mems = trees.map(refltree2cons)

    if(mems.exists(.isEmpty))
      None
    else
      Some(new ArrayConstant(
	mems.map(.get).toArray,
	symbolReifier.unreify(arrayType)))
  }


  /** Convert a constant to an equivalent reflect tree. */
  private def cons2refltree(cons: Constant): reflect.Tree = {
    import reflect._

    (cons: @unchecked) match {
      case acons:ArrayConstant =>
	val elems = acons.arrayValue.toList
	val arrayType = symbolReifier.reify(cons.tpe)
	val elemType: reflect.Type = arrayType match {
	  case AppliedType(_, List(et)) => et
	  case _ =>
	    assert(false, "array type is not an array type");
	    reflect.NoType
	}
        val elemTrees = elems map cons2refltree

        val arrayObject = reflect.This(reflect.Class("scala.Array"))

        // The following two gigantic trees were found by printing
        // out what the reifier makes.  If the reifier changes, they
        // should be updated.
        if(symbolReifier.unreify(elemType) <:< definitions.AnyValClass.tpe)
          Apply(Select(Select(Ident(Field("scala",PrefixedType(reflect.ThisType(RootSymbol),Class("scala")))),Field("scala.Array",PrefixedType(reflect.ThisType(Class("scala")),Class("scala.Array")))),Method("scala.Array.apply",reflect.MethodType(List(AppliedType(PrefixedType(reflect.ThisType(Class("scala")),Class("scala.<repeated>")),List(PrefixedType(reflect.ThisType(Class("scala")),Class("scala.Int"))))),AppliedType(PrefixedType(reflect.ThisType(Class("scala")),Class("scala.Array")),
          List(elemType))))), elemTrees)


	else
	  Apply(TypeApply(Select(Select(Ident(Field("scala",PrefixedType(reflect.ThisType(RootSymbol),Class("scala")))),Field("scala.Array",PrefixedType(reflect.ThisType(Class("scala")),Class("scala.Array")))),Method("scala.Array.apply",reflect.PolyType(List(reflect.NoSymbol),List((PrefixedType(reflect.ThisType(Class("scala")),Class("scala.Nothing")),PrefixedType(reflect.ThisType(Class("scala")),TypeField("scala.AnyRef",PrefixedType(reflect.ThisType(Class("java.lang")),Class("java.lang.Object")))))),reflect.MethodType(List(AppliedType(PrefixedType(reflect.ThisType(Class("scala")),Class("scala.<repeated>")),List(PrefixedType(reflect.NoType,reflect.NoSymbol)))),AppliedType(PrefixedType(reflect.ThisType(Class("scala")),Class("scala.Array")),List(PrefixedType(reflect.NoType,reflect.NoSymbol))))))),
         List(elemType)), elemTrees)

      case Constant(value) => reflect.Literal(value)
    }
  }





  /** An argument to an annotation.  It includes a parse tree,
   *  and it includes a compile-time constant for the tree if possible.
   */
  class AnnotationArgument(val tree: reflect.Tree)
  {
    def this(cons: Constant) = this(cons2refltree(cons))

//println("tree is: " + tree)

    val constant: Option[Constant] = refltree2cons(tree)

    def isConstant = !constant.isEmpty

    override def toString: String =
      constant match {
        case Some(cons) => cons.escapedStringValue
        case None => tree.toString
      }
  }

  /** Typed information about an annotation.  It can be attached to
   *  either a symbol or an annotated type.
   */
  case class AnnotationInfo(
    atp: Type,
    args: List[AnnotationArgument],
    assocs: List[(Name, AnnotationArgument)])
  {
    override def toString: String =
      atp +
      (if (args.isEmpty) ""
       else args.mkString("(", ", ", ")")) +
      (if (assocs.isEmpty) ""
       else (assocs map { case (x, y) => x+" = "+y } mkString ("{", ", ", "}")))

    /** Check whether all arguments and assocations are constants */
    def isConstant =
      ((args forall (.isConstant)) &&
       (assocs map (._2) forall (.isConstant)))
  }
}
