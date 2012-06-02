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
  InfoTransform with TypingTransformers with CPSUtils with TreeDSL {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.atOwner             // methods to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "selectivecps"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CPSTransformer(unit)

  /** This class does not change linearization */
  override def changesBaseClasses = false

  /** - return symbol's transformed type,
   */
  def transformInfo(sym: Symbol, tp: Type): Type = {
    if (!cpsEnabled) return tp

    val newtp = transformCPSType(tp)

    if (newtp != tp)
      debuglog("transformInfo changed type for " + sym + " to " + newtp);

    if (sym == MethReifyR)
      debuglog("transformInfo (not)changed type for " + sym + " to " + newtp);

    newtp
  }

  def transformCPSType(tp: Type): Type = {  // TODO: use a TypeMap? need to handle more cases?
    tp match {
      case PolyType(params,res) => PolyType(params, transformCPSType(res))
      case NullaryMethodType(res) => NullaryMethodType(transformCPSType(res))
      case MethodType(params,res) => MethodType(params, transformCPSType(res))
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
    private val patmatTransformer = patmat.newTransformer(unit)

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
          debuglog("found shift: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethShiftR) // TODO: correct?
            //gen.mkAttributedSelect(gen.mkAttributedSelect(gen.mkAttributedSelect(gen.mkAttributedIdent(ScalaPackage),
            //ScalaPackage.tpe.member("util")), ScalaPackage.tpe.member("util").tpe.member("continuations")), MethShiftR)
            //gen.mkAttributedRef(ModCPS.tpe,  MethShiftR) // TODO: correct?
            debuglog("funR.tpe = " + funR.tpe)
            Apply(
                TypeApply(funR, targs).setType(appliedType(funR.tpe, targs.map((t:Tree) => t.tpe))),
                args.map(transform(_))
            ).setType(transformCPSType(tree.tpe))
          }

        case Apply(TypeApply(fun, targs), args)
        if (fun.symbol == MethShiftUnit) =>
          debuglog("found shiftUnit: " + tree)
          atPos(tree.pos) {
            val funR = gen.mkAttributedRef(MethShiftUnitR) // TODO: correct?
            debuglog("funR.tpe = " + funR.tpe)
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
            debuglog("funR.tpe = " + funR.tpe)
            Apply(
                TypeApply(funR, targs).setType(appliedType(funR.tpe, targs.map((t:Tree) => t.tpe))),
                args.map(transform(_))
            ).setType(transformCPSType(tree.tpe))
          }

      case Try(block, catches, finalizer) =>
        // currently duplicates the catch block into a partial function.
        // this is kinda risky, but we don't expect there will be lots
        // of try/catches inside catch blocks (exp. blowup unlikely).

        // CAVEAT: finalizers are surprisingly tricky!
        // the problem is that they cannot easily be removed
        // from the regular control path and hence will
        // also be invoked after creating the Context object.

        /*
        object Test {
          def foo1 = {
            throw new Exception("in sub")
            shift((k:Int=>Int) => k(1))
            10
          }
          def foo2 = {
            shift((k:Int=>Int) => k(2))
            20
          }
          def foo3 = {
            shift((k:Int=>Int) => k(3))
            throw new Exception("in sub")
            30
          }
          def foo4 = {
            shift((k:Int=>Int) => 4)
            throw new Exception("in sub")
            40
          }
          def bar(x: Int) = try {
            if (x == 1)
              foo1
            else if (x == 2)
              foo2
            else if (x == 3)
              foo3
            else //if (x == 4)
              foo4
          } catch {
            case _ =>
              println("exception")
              0
          } finally {
            println("done")
          }
        }

        reset(Test.bar(1)) // should print: exception,done,0
        reset(Test.bar(2)) // should print: done,20 <-- but prints: done,done,20
        reset(Test.bar(3)) // should print: exception,done,0 <-- but prints: done,exception,done,0
        reset(Test.bar(4)) // should print: 4 <-- but prints: done,4
        */

        val block1 = transform(block)
        val catches1 = transformCaseDefs(catches)
        val finalizer1 = transform(finalizer)

        if (hasAnswerTypeAnn(tree.tpe)) {
          //vprintln("CPS Transform: " + tree + "/" + tree.tpe + "/" + block1.tpe)

          val (stms, expr1) = block1 match {
            case Block(stms, expr) => (stms, expr)
            case expr => (Nil, expr)
          }

          val targettp = transformCPSType(tree.tpe)

          val pos = catches.head.pos
          val funSym = currentOwner.newValueParameter(cpsNames.catches, pos).setInfo(appliedType(PartialFunctionClass.tpe, List(ThrowableClass.tpe, targettp)))
          val funDef = localTyper.typed(atPos(pos) {
            ValDef(funSym, Match(EmptyTree, catches1))
          })
          val expr2 = localTyper.typed(atPos(pos) {
            Apply(Select(expr1, expr1.tpe.member(cpsNames.flatMapCatch)), List(Ident(funSym)))
          })

          val exSym = currentOwner.newValueParameter(cpsNames.ex, pos).setInfo(ThrowableClass.tpe)

          import CODE._
          // generate a case that is supported directly by the back-end
          val catchIfDefined = CaseDef(
                Bind(exSym, Ident(nme.WILDCARD)),
                EmptyTree,
                IF ((REF(funSym) DOT nme.isDefinedAt)(REF(exSym))) THEN (REF(funSym) APPLY (REF(exSym))) ELSE Throw(REF(exSym))
              )

          val catch2 = localTyper.typedCases(List(catchIfDefined), ThrowableClass.tpe, targettp)
          //typedCases(tree, catches, ThrowableClass.tpe, pt)

          patmatTransformer.transform(localTyper.typed(Block(List(funDef), treeCopy.Try(tree, treeCopy.Block(block1, stms, expr2), catch2, finalizer1))))


/*
          disabled for now - see notes above

          val expr3 = if (!finalizer.isEmpty) {
            val pos = finalizer.pos
            val finalizer2 = duplicateTree(finalizer1)
            val fun = Function(List(), finalizer2)
            val expr3 = localTyper.typed(atPos(pos) { Apply(Select(expr2, expr2.tpe.member("mapFinally")), List(fun)) })

            val chown = new ChangeOwnerTraverser(currentOwner, fun.symbol)
            chown.traverse(finalizer2)

            expr3
          } else
            expr2
*/
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

              debuglog("found marked ValDef "+name+" of type " + vd.symbol.tpe)

              val tpe = vd.symbol.tpe
              val rhs1 = atOwner(vd.symbol) { transform(rhs) }
              rhs1.changeOwner(vd.symbol -> currentOwner) // TODO: don't traverse twice

              debuglog("valdef symbol " + vd.symbol + " has type " + tpe)
              debuglog("right hand side " + rhs1 + " has type " + rhs1.tpe)

              debuglog("currentOwner: " + currentOwner)
              debuglog("currentMethod: " + currentMethod)

              val (bodyStms, bodyExpr) = transBlock(rest, expr)
              // FIXME: result will later be traversed again by TreeSymSubstituter and
              // ChangeOwnerTraverser => exp. running time.
              // Should be changed to fuse traversals into one.

              val specialCaseTrivial = bodyExpr match {
                case Apply(fun, args) =>
                  // for now, look for explicit tail calls only.
                  // are there other cases that could profit from specializing on
                  // trivial contexts as well?
                  (bodyExpr.tpe.typeSymbol == Context) && (currentMethod == fun.symbol)
                case _ => false
              }

              def applyTrivial(ctxValSym: Symbol, body: Tree) = {

                val body1 = (new TreeSymSubstituter(List(vd.symbol), List(ctxValSym)))(body)

                val body2 = localTyper.typed(atPos(vd.symbol.pos) { body1 })

                // in theory it would be nicer to look for an @cps annotation instead
                // of testing for Context
                if ((body2.tpe == null) || !(body2.tpe.typeSymbol == Context)) {
                  //println(body2 + "/" + body2.tpe)
                  unit.error(rhs.pos, "cannot compute type for CPS-transformed function result")
                }
                body2
              }

              def applyCombinatorFun(ctxR: Tree, body: Tree) = {
                val arg = currentOwner.newValueParameter(name, ctxR.pos).setInfo(tpe)
                val body1 = (new TreeSymSubstituter(List(vd.symbol), List(arg)))(body)
                val fun = localTyper.typed(atPos(vd.symbol.pos) { Function(List(ValDef(arg)), body1) }) // types body as well
                arg.owner = fun.symbol
                body1.changeOwner(currentOwner -> fun.symbol)

                // see note about multiple traversals above

                debuglog("fun.symbol: "+fun.symbol)
                debuglog("fun.symbol.owner: "+fun.symbol.owner)
                debuglog("arg.owner: "+arg.owner)

                debuglog("fun.tpe:"+fun.tpe)
                debuglog("return type of fun:"+body1.tpe)

                var methodName = nme.map

                if (body1.tpe != null) {
                  if (body1.tpe.typeSymbol == Context)
                    methodName = nme.flatMap
                }
                else
                  unit.error(rhs.pos, "cannot compute type for CPS-transformed function result")

                debuglog("will use method:"+methodName)

                localTyper.typed(atPos(vd.symbol.pos) {
                  Apply(Select(ctxR, ctxR.tpe.member(methodName)), List(fun))
                })
              }

              def mkBlock(stms: List[Tree], expr: Tree) = if (stms.nonEmpty) Block(stms, expr) else expr

              try {
                if (specialCaseTrivial) {
                  debuglog("will optimize possible tail call: " + bodyExpr)

                  // FIXME: flatMap impl has become more complicated due to
                  // exceptions. do we need to put a try/catch in the then part??

                  // val ctx = <rhs>
                  // if (ctx.isTrivial)
                  //   val <lhs> = ctx.getTrivialValue; ...    <--- TODO: try/catch ??? don't bother for the moment...
                  // else
                  //   ctx.flatMap { <lhs> => ... }
                  val ctxSym = currentOwner.newValue(newTermName("" + vd.symbol.name + cpsNames.shiftSuffix)).setInfo(rhs1.tpe)
                  val ctxDef = localTyper.typed(ValDef(ctxSym, rhs1))
                  def ctxRef = localTyper.typed(Ident(ctxSym))
                  val argSym = currentOwner.newValue(vd.symbol.name).setInfo(tpe)
                  val argDef = localTyper.typed(ValDef(argSym, Select(ctxRef, ctxRef.tpe.member(cpsNames.getTrivialValue))))
                  val switchExpr = localTyper.typed(atPos(vd.symbol.pos) {
                    val body2 = mkBlock(bodyStms, bodyExpr).duplicate // dup before typing!
                    If(Select(ctxRef, ctxSym.tpe.member(cpsNames.isTrivial)),
                      applyTrivial(argSym, mkBlock(argDef::bodyStms, bodyExpr)),
                      applyCombinatorFun(ctxRef, body2))
                  })
                  (List(ctxDef), switchExpr)
                } else {
                  // ctx.flatMap { <lhs> => ... }
                  //     or
                  // ctx.map { <lhs> => ... }
                  (Nil, applyCombinatorFun(rhs1, mkBlock(bodyStms, bodyExpr)))
                }
              } catch {
                case ex:TypeError =>
                  unit.error(ex.pos, ex.msg)
                  (bodyStms, bodyExpr)
              }

            case _ =>
                val stm1 = transform(stm)
                val (a, b) = transBlock(rest, expr)
                (stm1::a, b)
            }
      }
    }


  }
}
