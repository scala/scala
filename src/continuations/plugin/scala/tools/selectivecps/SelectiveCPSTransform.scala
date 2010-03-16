// $Id$

package scala.tools.selectivecps

import scala.collection._

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.ast._

/**
 * In methods marked @cps, CPS-transform assignments introduced by ANF-transform phase.
 */
abstract class SelectiveCPSTransform extends PluginComponent with
  InfoTransform with TypingTransformers with CPSUtils {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.atOwner             // methods to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectivecps"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CPSTransformer(unit)


  /** - return symbol's transformed type,
   */
  def transformInfo(sym: Symbol, tp: Type): Type = {
    if (!cpsEnabled) return tp

    val newtp = transformCPSType(tp)

    if (newtp != tp)
      log("transformInfo changed type for " + sym + " to " + newtp);

    if (sym == MethReifyR)
      log("transformInfo (not)changed type for " + sym + " to " + newtp);

    newtp
  }

  def transformCPSType(tp: Type): Type = {  // TODO: use a TypeMap? need to handle more cases?
    tp match {
      case PolyType(params,res) => PolyType(params, transformCPSType(res))
      case MethodType(params,res) =>
        MethodType(params, transformCPSType(res))
      case TypeRef(pre, sym, args) => TypeRef(pre, sym, args.map(transformCPSType(_)))
      case _ =>
        getExternalAnswerTypeAnn(tp) match {
          case Some((res, outer)) =>
            appliedType(Context.tpe, List(removeAllCPSAnnotations(tp), res, outer))
          case _ =>
            removeAllCPSAnnotations(tp)
        }
    }
  }


  class CPSTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      if (!cpsEnabled) return tree
      postTransform(mainTransform(tree))
    }

    def postTransform(tree: Tree): Tree = {
      tree.setType(transformCPSType(tree.tpe))
    }


    def mainTransform(tree: Tree): Tree = {
      tree match {

        // TODO: can we generalize this?

        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethShift) =>
          log("found shift: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethShiftR) // TODO: correct?
            log(funR.tpe)
            Apply(
                TypeApply(funR, targs).setType(appliedType(funR.tpe, targs.map((t:Tree) => t.tpe))),
                args.map(transform(_))
            ).setType(transformCPSType(tree.tpe))
          }

        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethShiftUnit) =>
          log("found shiftUnit: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethShiftUnitR) // TODO: correct?
            log(funR.tpe)
            Apply(
                TypeApply(funR, List(targs(0), targs(1))).setType(appliedType(funR.tpe,
                    List(targs(0).tpe, targs(1).tpe))),
                args.map(transform(_))
            ).setType(appliedType(Context.tpe, List(targs(0).tpe,targs(1).tpe,targs(1).tpe)))
          }

        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethReify) =>
          log("found reify: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethReifyR) // TODO: correct?
            log(funR.tpe)
            Apply(
                TypeApply(funR, targs).setType(appliedType(funR.tpe, targs.map((t:Tree) => t.tpe))),
                args.map(transform(_))
            ).setType(transformCPSType(tree.tpe))
          }

      case Try(block, catches, finalizer) =>
        // currently duplicates the catch block into a partial function
        // this is kinda risky, but we don't expect there will be lots
        // of try/catches inside catch blocks (exp. blowup)

        val block1 = transform(block)
        val catches1 = transformCaseDefs(catches)
        val finalizer1 = transform(finalizer)

        if (block1.tpe.typeSymbol.tpe <:< Context.tpe) {
          //println("CPS Transform: " + tree)

          val pos = catches.head.pos

          val (stms, expr) = block1 match {
            case Block(stms, expr) => (stms, expr)
            case expr => (Nil, expr)
          }

          val arg = currentOwner.newValueParameter(pos, "$ex").setInfo(ThrowableClass.tpe)
          val catches2 = catches1 map (duplicateTree(_).asInstanceOf[CaseDef])
          val rhs = Match(Ident(arg), catches2)
          val fun = Function(List(ValDef(arg)), rhs)
          val expr2 = localTyper.typed(atPos(pos) { Apply(Select(expr, expr.tpe.member("flatCat")), List(fun)) })
          val block2 = treeCopy.Block(block1, stms, expr2)

          arg.owner = fun.symbol
          val chown = new ChangeOwnerTraverser(currentOwner, fun.symbol)
          chown.traverse(rhs)

          treeCopy.Try(tree, block2, catches1, finalizer1)
        } else {
          treeCopy.Try(tree, block1, catches1, finalizer1)
        }

      case Block(stms, expr) =>

          val (stms1, expr1) = transBlock(stms, expr)
          treeCopy.Block(tree, stms1, expr1)

        case _ =>
          super.transform(tree)
      }
    }



    def transBlock(stms: List[Tree], expr: Tree): (List[Tree], Tree) = {

      stms match {
        case Nil =>
          (Nil, transform(expr))

        case stm::rest =>

          stm match {
            case vd @ ValDef(mods, name, tpt, rhs)
            if (vd.symbol.hasAnnotation(MarkerCPSSym)) =>

              log("found marked ValDef "+name+" of type " + vd.symbol.tpe)

              val tpe = vd.symbol.tpe
              val rhs1 = transform(rhs)

              log("valdef symbol " + vd.symbol + " has type " + tpe)
              log("right hand side " + rhs1 + " has type " + rhs1.tpe)

              log("currentOwner: " + currentOwner)
              log("currentMethod: " + currentMethod)


              val (bodyStms, bodyExpr) = transBlock(rest, expr)

              val specialCaseTrivial = bodyExpr match {
                case Apply(fun, args) =>
                  // for now, look for explicit tail calls only.
                  // are there other cases that could profit from specializing on
                  // trivial contexts as well?
                  (bodyExpr.tpe.typeSymbol == Context) && (currentMethod == fun.symbol)
                case _ => false
              }

              def applyTrivial(ctxValSym: Symbol, body: Tree) = {

                new TreeSymSubstituter(List(vd.symbol), List(ctxValSym)).traverse(body)

                val body2 = localTyper.typed(atPos(vd.symbol.pos) { body })

                if ((body2.tpe == null) || !(body2.tpe.typeSymbol.tpe <:< Context.tpe)) {
                  println(body2 + "/" + body2.tpe)
                  unit.error(rhs.pos, "cannot compute type for CPS-transformed function result")
                }
                body2
              }

              def applyCombinatorFun(ctxR: Tree, body: Tree) = {
                val arg = currentOwner.newValueParameter(ctxR.pos, name).setInfo(tpe)
                new TreeSymSubstituter(List(vd.symbol), List(arg)).traverse(body)
                val fun = localTyper.typed(atPos(vd.symbol.pos) { Function(List(ValDef(arg)), body) }) // types body as well
                arg.owner = fun.symbol
                new ChangeOwnerTraverser(currentOwner, fun.symbol).traverse(body)

                log("fun.symbol: "+fun.symbol)
                log("fun.symbol.owner: "+fun.symbol.owner)
                log("arg.owner: "+arg.owner)

                log("fun.tpe:"+fun.tpe)
                log("return type of fun:"+body.tpe)

                var methodName = "map"

                if (body.tpe != null) {
                  if (body.tpe.typeSymbol.tpe <:< Context.tpe)
                    methodName = "flatMap"
                }
                else
                  unit.error(rhs.pos, "cannot compute type for CPS-transformed function result")

                log("will use method:"+methodName)

                localTyper.typed(atPos(vd.symbol.pos) {
                  Apply(Select(ctxR, ctxR.tpe.member(methodName)), List(fun))
                })
              }

              try {
                if (specialCaseTrivial) {
                  log("will optimize possible tail call: " + bodyExpr)
                  // val ctx = <rhs>
                  // if (ctx.isTrivial)
                  //   val <lhs> = ctx.getTrivialValue; ...
                  // else
                  //   ctx.flatMap { <lhs> => ... }
                  val ctxSym = currentOwner.newValue(vd.symbol.name + "$shift").setInfo(rhs1.tpe)
                  val ctxDef = localTyper.typed(ValDef(ctxSym, rhs1))
                  def ctxRef = localTyper.typed(Ident(ctxSym))
                  val argSym = currentOwner.newValue(vd.symbol.name).setInfo(tpe)
                  val argDef = localTyper.typed(ValDef(argSym, Select(ctxRef, ctxRef.tpe.member("getTrivialValue"))))
                  val switchExpr = localTyper.typed(atPos(vd.symbol.pos) {
                    val body2 = duplicateTree(Block(bodyStms, bodyExpr)) // dup before typing!
                    If(Select(ctxRef, ctxSym.tpe.member("isTrivial")),
                      applyTrivial(argSym, Block(argDef::bodyStms, bodyExpr)),
                      applyCombinatorFun(ctxRef, body2))
                  })
                  (List(ctxDef), switchExpr)
                } else {
                  // ctx.flatMap { <lhs> => ... }
                  //     or
                  // ctx.map { <lhs> => ... }
                  (Nil, applyCombinatorFun(rhs1, Block(bodyStms, bodyExpr)))
                }
              } catch {
                case ex:TypeError =>
                  unit.error(ex.pos, ex.msg)
                  (bodyStms, bodyExpr)
              }

            case _ =>
                val (a, b) = transBlock(rest, expr)
                (transform(stm)::a, b)
            }
      }
    }


  }
}
