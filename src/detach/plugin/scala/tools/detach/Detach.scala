/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.tools.detach

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform._

abstract class Detach extends PluginComponent
                         with Transform with TypingTransformers {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "detach"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new DetachTransformer(unit)

  // set with the `-P:detach:enable` plugin option (see DetachPlugin) */
  protected[detach] var isEnabled = false

  private class DetachTransformer(unit: CompilationUnit)
  extends TypingTransformer(unit) {
    private val DEBUG = settings.debug.value
    private val PROXY_PREFIX  = "proxy$"  // local proxy objects
    private val PROXY_SUFFIX  = "$proxy"  // top-level proxy classes
    private val DETACH_SUFFIX = "$detach" // detached closures
    private val IMPL_SUFFIX   = "Impl"    // follows Java convention

    private val nme_bind = newTermName("bind")
    private val nme_unbind = newTermName("unbind")
    private val nme_unreferenced = newTermName("unreferenced")

    private val Functions = FunctionClass.toList // see method isFuncType

    private val RemoteClass =
      definitions.getClass("java.rmi.Remote")

    private val UIDClass =
      definitions.getClass("java.rmi.server.UID")

    private val UnicastRemoteObjectClass =
      definitions.getClass("java.rmi.server.UnicastRemoteObject")

    private val UnreferencedClass =
      definitions.getClass("java.rmi.server.Unreferenced")

    private val DetachModule =
      definitions.getModule("scala.remoting.detach")

    private val DebugModule =
      definitions.getModule("scala.remoting.Debug")

    private val RemoteRefModule =
      definitions.getModule("scala.runtime.RemoteRef")

    private val ThreadModule =
      definitions.getModule("java.lang.Thread")

    private val UnicastRemoteObjectModule =
      definitions.getModule("java.rmi.server.UnicastRemoteObject")

    private val remoteAnnotationInfo = {
      val RemoteAttr: Symbol = definitions.getClass("scala.remote")
      AnnotationInfo(RemoteAttr.tpe, List(), List())
    }

    private val serializableAnnotationInfo =
      AnnotationInfo(SerializableAttr.tpe, List(), List())
/*
    private val throwsAnnotationInfo = {
      val RemoteExceptionClass = definitions.getClass("java.rmi.RemoteException")
      val ThrowsAttr = definitions.getClass("scala.throws")
      AnnotationInfo(
        ThrowsAttr.tpe,
        List(Literal(Constant(RemoteExceptionClass.tpe))),
        List()
      )
    }
*/
    // todo: see generation of Java version UID
    private def serialVersionUIDAnnotationInfo(clazz: Symbol) = {
      def genHash(sym: Symbol): Long = {
        val sym1 = if (sym.isConstructor) sym.owner else sym
        val ts = sym.tpe match {
          case MethodType(params, rt) => (params map (_.tpe)) ::: List(rt)
          case t => List(t)
        }
        val hashes = sym1.nameString.hashCode ::
          (ts map (_.typeSymbol.nameString.hashCode))
        (0L /: hashes)((acc, h) => acc ^ h)
      }
      val hashes = for (sym <- clazz.info.decls.toList) yield genHash(sym)
      val uid: Long = (0L /: hashes) ((acc, h) => acc * 41 + h)
      val serialVersionUIDAttr = definitions.getClass("scala.SerialVersionUID")
      AnnotationInfo(
        serialVersionUIDAttr.tpe,
        List(Literal(Constant(uid))),
        List()
      )
    }

    private def elems(suffix: String): List[(Symbol, Symbol)] =
      for (clazz <- ObjectRefClass :: refClass.valuesIterator.toList) yield {
        val name = "scala.runtime.remoting.Remote" + clazz.name + suffix
        (clazz, definitions.getClass(name))
      }
    private val remoteRefClass = immutable.HashMap(elems(""): _*)
    private val remoteRefImpl = immutable.HashMap(elems("Impl"): _*)

    private val proxyInterfaceDefs = new mutable.HashMap[Symbol/*owner*/, ListBuffer[Tree]]
    private val detachedClosureApply = new mutable.HashMap[Tree, Apply]

    private type SymSet = mutable.HashSet[Symbol]
    private val capturedObjects = new mutable.HashMap[Symbol/*clazz*/, SymSet]
    private val capturedFuncs = new mutable.HashMap[Symbol/*clazz*/, SymSet]
    private val capturedCallers = new mutable.HashMap[Symbol/*clazz*/, SymSet]
    private val capturedThisClass = new mutable.HashMap[Symbol, Symbol]

    private val proxies = new mutable.HashMap[
      Symbol, //clazz
      (Symbol, Symbol, mutable.HashMap[Symbol, Symbol]) //iface, impl, accessor map
    ]
    def toInterface(clazz: Symbol) = proxies(clazz)._1
    private val classdefs = new mutable.HashMap[Symbol/*clazz*/, ClassDef]
    // detachedClosure gathers class definitions containing a "detach" apply
    private val detachedClosure = new mutable.HashMap[Symbol/*clazz*/, ClassDef]

    /** <p>
     *    The method <code>freeObjTraverser.traverse</code> is invoked
     *    in the method <code>DetachPlugin.transformUnit</code> in order to
     *    gather information about objects referenced inside a detached
     *    closure and which will be accessed remotely through object proxies.
     *  </p>
     *  <p>
     *    Object proxies are generated in method <code>mkClosureApply</code>
     *    and their definitions are generated in method <code>genProxy</code>.
     *  </p>
     */
    private val freeObjTraverser = new Traverser {
      def symSet(f: mutable.HashMap[Symbol, SymSet], sym: Symbol): SymSet = f.get(sym) match {
        case Some(ss) => ss
        case None => val ss = new mutable.HashSet[Symbol]; f(sym) = ss; ss
      }
      def getClosureApply(tree: Tree): Apply = tree match {
        case Block(_, expr) => getClosureApply(expr)
        case Typed(expr, _) => getClosureApply(expr)
        case apply @ Apply(Select(_, _), _) => apply // sel="<init>" or some "f$0"
        case Apply(fun, _)  => getClosureApply(fun)
        case _ =>
          throw new Error("getClosureApply: unhandled case " + tree)
      }
      def isFuncType(tp: Type): Boolean = tp match {
        case TypeRef(pre, sym, args) =>
          Functions contains sym.tpe.typeSymbol
        case _ =>
          false
      }
      def isOuterMember(sym: Symbol): Boolean =
        sym.isOuterAccessor ||
        sym.name.endsWith(nme.OUTER/*, nme.OUTER.length*/)
      override def traverse(tree: Tree) {
        val sym = tree.symbol
        val owner =
          if (currentOwner.isModule) currentOwner
          else currentOwner.enclClass
        tree match {
          case cdef @ ClassDef(_, _, _, impl) =>
            classdefs(sym) = cdef
            super.traverse(impl)
            if (detachedClosure contains sym) {
              detachedClosure(sym) = cdef
              symSet(capturedObjects, sym) += capturedThisClass(sym)
            }

          case Apply(Select(qual, _), List(arg))
          if (qual.tpe <:< DetachModule.tpe) =>
            assert(isFuncType(arg.tpe))//debug
            val t = getClosureApply(arg)
            if (!t.fun.symbol.isConstructor)
              unit.error(t.pos, "detach inapplicable for " +t.fun.symbol)
            val sym = t.fun.symbol.owner
            capturedThisClass(sym) = owner
            symSet(capturedFuncs, sym)
            detachedClosureApply(tree) = t
            classdefs get sym match {
              case None =>
                detachedClosure(sym) = null // set later in case ClassDef
              case Some(cdef) =>
                detachedClosure(sym) = cdef
                symSet(capturedObjects, sym) += capturedThisClass(sym)
            }
            super.traverse(arg)

          case Select(qual @ This(_), name)
          if qual.symbol.isModuleClass && !qual.symbol.isPackageClass =>
            val qsym = qual.symbol
            symSet(capturedFuncs, owner) += sym
            symSet(capturedObjects, owner) += qsym

          case Select(qual, name)
          if (qual.hasSymbol &&
              (sym.owner != owner) &&
              !(sym.ownerChain contains ScalaPackageClass) &&
              !(sym.owner hasFlag JAVA)) =>
            val qsym = qual.symbol
            symSet(capturedFuncs, owner) += sym
            if (qsym.isStaticModule && !qsym.isPackage) {
              //println("*****1******* capturedObjects("+owner+") += "+qsym)
              symSet(capturedObjects, owner) += qsym
            }
            else if (!isOuterMember(qsym) && !(qsym isNestedIn owner)) {
              //println("*****3******* capturedCallers("+sym+") += "+qsym)
              symSet(capturedCallers, sym) += qsym
            }

          case _ =>
            super.traverse(tree)
        }
      }
    } //freeObjTraverser

    private val valueClass = immutable.HashMap(
      (for ((sym, ref) <- refClass.toList) yield (ref, sym)): _*
    ) + (ObjectRefClass -> ObjectClass)

    private def toValueClass(tp: Type): Type =
      if (isRefClass(tp)) valueClass(tp.typeSymbol).tpe
      else if (proxies contains tp.typeSymbol) toInterface(tp.typeSymbol).tpe
      else tp

    private def isRefClass(tp: Type): Boolean =
      (tp ne null) &&
      ((refClass.valuesIterator contains tp.typeSymbol) || (ObjectRefClass eq tp.typeSymbol))

    private def isRemoteRefClass(tp: Type): Boolean =
      (tp ne null) && (remoteRefClass.valuesIterator contains tp.typeSymbol)

    private def mkRemoteRefClass(tp: Type): Type = {
      assert(isRefClass(tp))
      val tp1 = remoteRefClass(tp.typeSymbol)
      typeRef(tp1.typeConstructor.prefix, tp1, Nil) // after erasure, no type anymore!
    }

    class TreeOuterSubstituter(from: List[Symbol], to: List[Symbol]) extends Traverser {
      if (DEBUG)
        println("\nTreeOuterSubstituter:"+
                "\n\tfrom="+from.mkString(",")+
                "\n\tto="+to.mkString(","))
      val substMap = new mutable.HashMap[Symbol, Symbol]
      override def traverse(tree: Tree) {
        def subst(from: List[Symbol], to: List[Symbol]) {
          if (!from.isEmpty)
            if (tree.symbol.tpe == from.head.tpe) {
              if (DEBUG)
                println("\nTreeOuterSubstituter\n\tsym="+tree.symbol+
                        ", tpe="+tree.symbol.tpe+
                        "\n\towner="+tree.symbol.owner)
              tree.symbol updateInfo to.head.tpe
            }
            else tree.symbol.tpe match {
              case MethodType(params, restp) =>
                for (p <- params if p.tpe == from.head.tpe) {
                  p updateInfo to.head.tpe
                }
                if (restp == from.head.tpe) {
                  if (DEBUG)
                    println("\nTreeOuterSubstituter(2)\n\tsym="+tree.symbol+
                            ", tpe="+tree.symbol.tpe+
                            ", owner="+tree.symbol.owner)
                  tree.symbol updateInfo MethodType(params, to.head.tpe)
                }
              case _ =>
                subst(from.tail, to.tail)
            }
        }
        def isOuter(sym: Symbol): Boolean =
          sym.isOuterAccessor ||
          sym.name.endsWith(nme.OUTER/*, nme.OUTER.length*/)
        if (tree.hasSymbol && isOuter(tree.symbol)) subst(from, to)
        super.traverse(tree)
      }
    }

    // based on class Trees.TreeTypeSubstituter
    private class TreeTypeRefSubstituter(clazz: Symbol) extends Traverser {
      override def traverse(tree: Tree) {
        val sym = tree.symbol
        if (tree.hasSymbol && isRefClass(sym.tpe) &&
           (sym.owner.enclClass == clazz) &&
           (sym.isValueParameter || sym.hasFlag(PARAMACCESSOR))) {
          sym setInfo mkRemoteRefClass(sym.tpe)
          tree.tpe = sym.tpe
        }
        if (isRefClass(tree.tpe))
          tree.tpe = mkRemoteRefClass(tree.tpe)
        super.traverse(tree)
      }
      override def apply[T <: Tree](tree: T): T = super.apply(tree)
    }

    private class TreeOwnerSubstituter(from: Symbol, to: Symbol) extends Traverser {
      def substType(sym: Symbol): Type = {
        def subst(tpe: Type): Type = tpe match {
          case MethodType(params, restp) =>
            println("TreeOwnerSubstituter[1]: tpe="+tpe+
                    ", tpe.typeSymbol="+tpe.typeSymbol+", sym="+sym)//debug
            for (p <- params if p.tpe == from.tpe) {
              println("TreeOwnerSubstituter[2]: sym="+sym+
                      ", sym.owner="+sym.owner+", p.tpe="+p.tpe)//debug
              p updateInfo to.tpe
            }
            MethodType(params, subst(restp))
          case _ =>
            if (sym.owner == from && tpe == from.tpe) {
              println("TreeOwnerSubstituter[3]: sym="+sym+
                      ", owner="+sym.owner+", tpe="+tpe)//debug
              to.tpe
            } else tpe
        }
        subst(sym.tpe)
      }
      val map = new mutable.HashMap[Symbol, Symbol]
      override def traverse(tree: Tree) {
        if (tree.hasSymbol && tree.symbol != NoSymbol) {
          val sym = tree.symbol
          if (sym.owner == from) {
            val sym1 = map get sym match {
              case Some(s) => s
              case None => val s = sym.cloneSymbol(to); map(sym) = s; s
            }
            tree setSymbol sym1
          }
          val sym1 = tree.symbol
          val tp = substType(sym1)
          if (tp != sym1.tpe) {
            if (sym1.owner == to)
              println("\n%%%%%1%%%%%%% TreeOwnerSubst: tree="+tree+", sym1="+sym1+", sym1.owner="+sym1.owner)//debug
            sym1 setInfo tp
            tree setSymbol sym1
          }
        }
        super.traverse(tree)
      }
      //override def apply[T <: Tree](tree: T): T = super.apply(tree/*.duplicate*/)
    }

    private var inConstructorFlag = 0L

    private def isCaptured(clazz: Symbol, sym: Symbol): Boolean =
      if (capturedFuncs contains clazz) {
        //log("**1** isCaptured: clazz="+clazz+", sym="+sym+", ")
        capturedFuncs(clazz) contains sym
      }
      else {
        //log("**2** isCaptured: clazz="+clazz+", sym="+sym)
        sym.isMethod && !sym.isConstructor
      }

    private class TreeAccessorSubstituter(clazz: Symbol, objs: List[Symbol], proxySyms: List[Symbol])
    extends Transformer {
      def removeAccessors(tree: Tree): Tree = tree match {
        case Apply(fun, _) =>
          removeAccessors(fun)
        case Select(qual, _) if tree.hasSymbol && tree.symbol.isOuterAccessor =>
          removeAccessors(qual)
        case _ =>
          tree
      }
      if (DEBUG)
        println("\nTreeAccessorSubstituter: "+
                "\n\tobjs="+objs.mkString(",")+
                "\n\tproxies="+proxySyms.mkString(","))
      override def transform(tree: Tree): Tree = tree match {
        // transforms field assignment $outer.i$1.elem=..
        // into setter $outer.i$1_=(..)
        case Assign(lhs @ Select(qual1 @ Select(qual, name), name1), rhs)
        if qual1.hasSymbol && !qual1.symbol.isPrivateLocal &&
           isRemoteRefClass(qual1.tpe) =>
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Assign1\n\tqual1="+qual1+", sel.tpe="+lhs.tpe+
                    "\n\tqual1.tpe="+qual1.tpe+", name1="+name1+
                    "\n\tqual.tpe="+qual.tpe+", tree.tpe="+tree.tpe)//debug
          val iface = toInterface(qual.tpe.typeSymbol)
          val sym = iface.tpe.decls lookup nme.getterToSetter(name)
          atPos(tree.pos)(Apply(
            Select(super.transform(qual), sym) setType lhs.tpe,
            List(super.transform(rhs))
          ) setType tree.tpe)

        // transforms local assignment this.x$1.elem=..
        // into setter method this.x$1_=(..)
        case Assign(lhs @ Select(qual, name), rhs)
        if qual.hasSymbol && qual.symbol.isPrivateLocal &&
           isRemoteRefClass(qual.tpe) =>
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Assign2"+
                    "\n\tqual="+qual+", qual.tpe="+qual.tpe+
                    "\n\tname="+name)
          // substitute the 'elem' member of the reference class with
          // the corresponding setter method of the remote reference class.
          val qual1 = super.transform(qual)
          val sym = qual1.tpe.decls lookup nme.getterToSetter(name)
          val fun = gen.mkAttributedSelect(qual1, sym)
          Apply(fun, List(super.transform(rhs))) setType lhs.tpe

        case Assign(Select(qual, name), rhs)
        if qual.hasSymbol && (objs contains qual.symbol) =>
          val sym = qual.symbol
          val proxy = proxySyms(objs indexOf sym)
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Assign3"+
                    "\n\tqual="+qual+", qual.tpe="+qual.tpe+
                    "\n\tproxy="+proxy+", proxy.tpe="+proxy.tpe+
                    "\n\tname="+name)//debug
          // substitute the member accessor of the enclosing class with
          // the corresponding setter method of the detached interface.
          val iface = toInterface(sym)
          val substSymbols = new TreeSymSubstituter(
            sym.info.decls.toList filter { isCaptured(sym, _) },
            iface.info.decls.toList)
          substSymbols(Apply(
                         Select(Ident(proxy), nme.getterToSetter(name)),
                         List(super.transform(rhs))))

        // transforms setter invocation this.i$1_=(..)
        // into setter invocation $outer.i$1_=(..)
        case Apply(Select(qual @ This(_), name), args)
        if (objs contains qual.symbol) && nme.isSetterName(name) =>
          val proxy = proxySyms(objs indexOf qual.symbol)
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Apply"+
                    "\n\tqual="+qual+", qual.tpe="+qual.tpe+
                    "\n\tproxy="+proxy+", proxy.tpe="+proxy.tpe+
                    "\n\tname="+name+", decoded="+name.decode)
          val qual1 = gen.mkAttributedSelect(gen.mkAttributedThis(proxy.owner), proxy)
          val sym1 = proxy.info.decls lookup name.decode
          val fun = gen.mkAttributedSelect(qual1, sym1)
          Apply(fun, args map (super.transform(_))) setType tree.tpe

        // transforms access to field this.name$1
        // into invocation of getter method $outer.name$1()
        case Select(qual @ This(_), name)
        if objs contains qual.symbol =>
          val proxy = proxySyms(objs indexOf qual.symbol)
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Select"+
                    "\n\tqual="+qual+", qual.tpe="+qual.tpe+
                    "\n\tproxy="+proxy+", proxy.tpe="+proxy.tpe+
                    "\n\tname="+name+", decoded="+name.decode)
          val qual1 = gen.mkAttributedSelect(gen.mkAttributedThis(proxy.owner), proxy)
          val sym1 = proxy.info.decls lookup nme.originalName(name) //name
          gen.mkAttributedSelect(qual1, sym1)

        // transforms field $outer.name$1 into getter method $outer.name$1()
        case Select(qual @ Select(_, name1), name)
        if qual.hasSymbol && name1.endsWith(nme.OUTER/*, nme.OUTER.length*/) &&
           !tree.symbol.isMethod =>
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Select0\n\tqual="+qual+
                    ", qual.tpe="+qual.tpe+", name="+name)//debug
          val sym = qual.symbol
          val qual1 = gen.mkAttributedSelect(gen.mkAttributedThis(sym.owner), sym)
          val iface = toInterface(qual.tpe.typeSymbol)
          val sym1 = iface.tpe.decls lookup name
          val fun = gen.mkAttributedSelect(qual1, sym1)
          Apply(fun, List()) setType tree.tpe

        case Select(apply @ Apply(fun @ Select(qual, _), _), name)
        if fun.symbol.isOuterAccessor =>
          val tsym = fun.symbol.tpe.resultType.typeSymbol
          val funcs = capturedFuncs(clazz).toList filter (sym =>
            (tsym.ownerChain contains sym.owner) || (tsym isSubClass sym.owner))
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Select1\n\tfun="+fun+
                    ",\n\tfun.tpe="+fun.tpe+", name="+name+
                    ",\n\tfuncs="+funcs)//debug
          funcs find (tree.symbol.==) match {
            case Some(sym) =>
              val qual1 =
                if (currentOwner.enclClass isNestedIn clazz) apply
                else removeAccessors(qual)
              val name1 =
                (if (tsym isSubClass qual1.tpe.typeSymbol) ""
                 else tsym.fullName('$')+"$")+sym.name
              val iface = toInterface(qual1.tpe.typeSymbol)
              val sym1 = iface.tpe.decls lookup name1
              gen.mkAttributedSelect(qual1, sym1)
            case None =>
              super.transform(tree)
          }

        // transforms field access $outer.i$1.elem
        // into invocation of getter method $outer.i$1()
        case Select(qual @ Select(qual1, name1), name)
        if qual.hasSymbol && !qual.symbol.isPrivateLocal &&
           isRemoteRefClass(qual.tpe) =>
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Select2\n\tqual="+qual+
                    "\n\tqual.tpe="+qual.tpe+", tree.tpe="+tree.tpe)//debug
          val iface = toInterface(qual.symbol.owner)
          val sym1 = iface.tpe.decls lookup name1
          val fun = gen.mkAttributedSelect(qual1, sym1)
          Apply(fun, List()) setType tree.tpe

        // transforms local access this.i$1.elem
        // into invocation of getter method this.i$1()
        case Select(qual, name)
        if qual.hasSymbol && qual.symbol.isPrivateLocal &&
           isRemoteRefClass(qual.tpe) =>
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Select3\n\tqual="+qual+
                    "\n\tqual.tpe="+qual.tpe)//debug
          val sym = qual.tpe.decls lookup name
          val fun = gen.mkAttributedSelect(qual, sym)
          Apply(fun, List()) setType tree.tpe

        case Select(qual, name)
        if qual.hasSymbol && (objs contains qual.symbol) =>
          if (DEBUG)
            println("\nTreeAccessorSubstituter: Select4\n\tqual="+qual+
                    ", qual.tpe="+qual.tpe+", name="+name)//debug
          val sym = qual.symbol
          val proxy = proxySyms(objs indexOf sym)
          // substitute the accessor of a member of the enclosing class
          // with the corresponding accessor of the detached interface
          val qual1 = gen.mkAttributedSelect(gen.mkAttributedThis(proxy.owner), proxy)
          val iface = toInterface(sym)
          val sym1 = iface.tpe.decls lookup name.decode
          gen.mkAttributedSelect(qual1, sym1)

        case _ =>
          super.transform(tree)
      }
      def apply[T <: Tree](tree: T): T = transform(tree).asInstanceOf[T]
    } // TreeAccessorSubstituter
/*
    private class TreeNameSubstituter(from: Name, to: Symbol) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Super(qual, mix) if tree.symbol.name == from =>
          Super(qual, mix) setSymbol to
        case This(name) if name == from =>
          This(to.name) setSymbol to
        case _ =>
          super.transform(tree)
      }
      def apply[T <: Tree](tree: T): T = transform(tree).asInstanceOf[T]
    }
*/
    /** <p>
     *    Given the closure definition (generated by previous phases)
     *  </p><pre>
     *    class $anonfun$1 extends Object with Function1 {
     *      def this($outer: C, x$1: Int): $anonfun$1 = ..
     *      def apply(x: Int): Int = x + this.$outer.x() + this.x$1
     *    }</pre>
     *  <p>
     *    the method <code>mkClosureDef</code> transforms the above code
     *    to the following:
     *  </p><pre>
     *    @serializable
     *    class $anonfun$1$detach extends Object with Function1 {
     *      def this($outer: C$proxy, x$1: Int): $anonfun$1$detach = ..
     *      def apply(x: Int): Int = x + this.$outer.x() + this.x$1
     *    }</pre>
     *  <p>
     *    In particular, it performs the following operations:
     *    1) add constructor parameter <code>proxy_n</code> to access
     *       proxy of the enclosing class
     *    2) change reference types in constructor arguments to type
     *       <code<Remote_type_Ref</code>'
     *    3) change occurences of <code>this</code> identifier to
     *        <code>proxy_n</code> in template code
     *    4) change reference types of local value definitions associated
     *       to updated constructor arguments to type <code>Remote_type_Ref</code>
     *  </p>
     *
     *  @param  clazz the symbol of the original closure definition
     *  @return the typed class definition for the detached closure.
     */
    private def mkClosureDef(clazz: Symbol): Tree = {
      val cdef = detachedClosure(clazz)
      val name = cdef.symbol.name
      if (name endsWith DETACH_SUFFIX)
        return cdef // closure already detached

      clazz.name = encode(clazz.name.decode + DETACH_SUFFIX)
      clazz addAnnotation serialVersionUIDAnnotationInfo(clazz)
      clazz addAnnotation serializableAnnotationInfo

      val thiz = capturedThisClass(clazz)
      val (List(outer), captured) =
        capturedObjects(clazz).toList partition (thiz.==)

      /** <p>
       *    Method <code>updateConstructorParams</code> updates the class
       *    symbol of the detached closure as follows:
       *    1) it appends the "$detach" suffix to the class name,
       *    2) it adds the "@serializable" annotation to class attributes,
       *    3) it adds a parameter symbol for each element of "captured".
       *  </p>
       *  <p>
       *    and also updates the signature of the constructor symbol:
       *    1) it adds a parameter type for each element of "captured",
       *    2) it changes reference types to remote reference types.
       *  </p>
       */
      def updateConstructorParams(vparams: List[ValDef]): List[Symbol] = {
        val hasOuter = !vparams.isEmpty && (vparams.head.symbol.tpe == thiz.tpe)
        val ctor = clazz.primaryConstructor
        val params = (for (sym <- captured) yield {
          val iface = toInterface(sym)
          val param = ctor.newValueParameter(ctor.pos, freshProxyName)
            .setFlag(SYNTHETIC)
            .setInfo(iface.tpe)
          param.owner = ctor
          param
        }) ::: (
          if (hasOuter) Nil
          else {
            val iface = toInterface(thiz)
            val param = ctor.newValueParameter(ctor.pos, nme.OUTER)
              .setFlag(SYNTHETIC)
              .setInfo(iface.tpe)
            param.owner = ctor
            List(param)
          }
        )
        val tp = ctor.tpe match {
          case mt @ MethodType(params1, restp) =>
            val params2 = if (hasOuter) {
              val iface = toInterface(params1.head.tpe.typeSymbol)
              ctor.newSyntheticValueParam(iface.tpe) :: params1.tail
            }
            else params1
            for (p <- params2 if isRefClass(p.tpe)) {
              p updateInfo mkRemoteRefClass(p.tpe)
            }
            MethodType(params ::: params2, restp)
          case tp =>
            tp
        }
        ctor updateInfo tp
        params
      } //updateConstructorParams

      /**
       */
      def updateConstructorDef(ctor: DefDef): (List[Tree], List[Symbol]) = {
        val DefDef(mods, name, tparams, List(vparams), tpt, rhs) = ctor
        val newparams = updateConstructorParams(vparams)
        val vparams0 = newparams map (sym => ValDef(sym) setType sym.tpe)
        val ctorDef = treeCopy.DefDef(ctor, mods, name, tparams, List(vparams0 ::: vparams), tpt, rhs)
        val accessors = for (sym <- newparams) yield {
          val acc = clazz.newValue(sym.pos, sym.name)
            .setFlag(SYNTHETIC | PARAMACCESSOR | PRIVATE | LOCAL)
            .setInfo(sym.tpe)
          clazz.info.decls enter acc
          acc
        }
        val accDefs = accessors map (sym => ValDef(sym) setType sym.tpe)
        (ctorDef :: accDefs, accessors)
      } //updateConstructorDef

      val impl = cdef.impl
      val (List(ctor: DefDef), body1) = impl.body partition (t =>
        t.isDef && t.symbol.isPrimaryConstructor)
      val (defs, accessors) = updateConstructorDef(ctor)
      val impl1 = treeCopy.Template(impl, impl.parents, impl.self, defs ::: body1)
      val (from, to) = /*List.unzip*/(
        for (obj <- captured ::: List(outer))
        yield (obj, toInterface(obj))
      ) unzip
      //val substNames = new TreeNameSubstituter(name, clazz)
      val substTypeRefs = new TreeTypeRefSubstituter(clazz)
      val substAccs = new TreeAccessorSubstituter(clazz, from, accessors)
      val substTypes = new TreeOuterSubstituter(from, to)
      val substSyms = new TreeSymSubstituter(from, to)
      val t1 = ClassDef(clazz, substSyms(substTypes(substAccs(substTypeRefs(impl1)))))
      //println("mkClosureDef: t(untyped)=\n"+nodeToString(t1))
      val t = localTyper typed t1
      detachedClosure(clazz) = t.asInstanceOf[ClassDef]
      //println("mkClosureDef: t(typed)=\n"+nodeToString(t))
      t
    } //mkClosureDef

    /** <p>
     *   Given a class <code>C</code> with member <code>x</code>
     *   which is (remotely) referenced from inside a detached closure:
     *  </p><pre>
     *    class C extends .. {
     *      var x: Int
     *    }</pre>
     *  <p>
     *    the method <code>addProxy</code> generates the following two
     *    proxy definitions (used later in method <code>mkClosureApply</code>
     *    to generate object proxies):
     *  </p><pre>
     *    trait C$proxy extends java.rmi.Remote {
     *      def x(): Int
     *      def x_=(x$1: Int): Unit
     *    }
     *    class C$proxyImpl
     *    extends java.rmi.server.UnicastRemoteObject
     *    with C$proxy with java.rmi.server.Unreferenced {
     *      def this(x$0: String, x$1: C): C$ProxyImpl = ..
     *      def x(): Int = this.x$1.x()
     *      def x_=(x$1: Int): Unit = this.x$1.x_=(x$1)
     *      def unreferenced(): Unit = RemoteRef.unbind(this.x$0)
     *    }</pre>
     */
    private def addProxy(closure: Symbol, clazz: Symbol) {
      // the Sun RMI compiler crashes with the error message
      // "error: An error has occurred in the compiler; ..." with trace
      // "sun.tools.java.CompilerError: getInnerClassField" if the
      // generated proxy class does not belong to the top-level scope.
      val proxyOwner = clazz.toplevelClass.owner //clazz.owner

      if (DEBUG)
        println("\nadd proxy for "+clazz+" in "+proxyOwner)//debug

      val (proxyIntf, proxyImpl, proxyMap) = proxies get clazz match {
        case Some(proxy) =>
          proxy
        case None =>
          val iface =
            proxyOwner.newClass(clazz.pos, encode(clazz.name.decode + PROXY_SUFFIX))
          iface.sourceFile = clazz.sourceFile
          iface setFlag (ABSTRACT | TRAIT | INTERFACE) // Java interface
          val iparents = List(ObjectClass.tpe, RemoteClass.tpe, ScalaObjectClass.tpe)
          iface setInfo ClassInfoType(iparents, newScope, iface)
          // methods must throw RemoteException
          iface addAnnotation remoteAnnotationInfo

          val iclaz =
            proxyOwner.newClass(clazz.pos, encode(iface.name.decode + IMPL_SUFFIX))
          iclaz.sourceFile = clazz.sourceFile
          iclaz setFlag (SYNTHETIC | FINAL)
          // Variant 1: rebind/unbind
          val cparents = List(UnicastRemoteObjectClass.tpe, iface.tpe,
                              UnreferencedClass.tpe, ScalaObjectClass.tpe)
          // Variant 2: un-/exportObject
          //val cparents = List(ObjectClass.tpe, iface.tpe,
          //                    UnreferencedClass.tpe, ScalaObjectClass.tpe)
          iclaz setInfo ClassInfoType(cparents, newScope, iclaz)
          val proxy = (iface, iclaz, new mutable.HashMap[Symbol, Symbol])
          proxies(clazz) = proxy
          proxy
      }

      def addAccessors() {
        def mkGetter(sym: Symbol, name: String): Symbol = {
          val getter = if (sym.isMethod) {
            val meth = sym.cloneSymbol(proxyIntf)
            meth.name = name
            val tsym = meth.tpe.resultType.typeSymbol
            if (proxies contains tsym)
              meth updateInfo MethodType(List(), toInterface(tsym).tpe)
            meth
          }
          else {
            val meth = proxyIntf.newMethod(sym.pos, nme.getterName(sym.originalName))
            meth setFlag ACCESSOR
            meth setInfo MethodType(List(), toValueClass(sym.tpe))
            meth
          }
          getter setFlag ABSTRACT
          getter resetFlag FINAL
          getter
        }
        def mkSetter(sym: Symbol): Symbol = {
          val setter = proxyIntf.newMethod(sym.pos, nme.getterToSetter(sym.originalName))
          setter setFlag (sym.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED)
          val param = setter.newSyntheticValueParam(toValueClass(sym.tpe))
          setter setInfo MethodType(List(param), UnitClass.tpe)
          setter setFlag ABSTRACT
          setter resetFlag FINAL
          setter
        }
        def create(owner: Symbol, clazz: Symbol) {
          val funcs = capturedFuncs(owner).toList
          funcs find (_.isConstructor) match {
            case Some(sym) if capturedFuncs contains sym.owner =>
              create(sym.owner, clazz)
            case _ =>
          }
          val newfuncs = funcs filterNot (proxyMap.valuesIterator.toList contains)
          val (members, others) = newfuncs partition (clazz isSubClass _.owner)
          val outers = others filter (sym =>
            (clazz isNestedIn sym.owner) && clazz.isClass)
          for (sym <- outers) {
            val sym1 = mkGetter(sym, sym.fullName('$'))
            proxyIntf.info.decls enter sym1
            proxyMap(sym1) = sym
          }/*
          for (sym <- outers if capturedCallers contains sym;
               caller <- capturedCallers(sym)) {
            val sym1 = mkGetter(sym, caller.nameString+'$'+sym.nameString)
            if (clazz.isAnonymousClass)
              println("[2] clazz="+clazz+", sym1="+sym1)
            proxyIntf.info.decls enter sym1
            proxyMap(sym1) = sym
          }*/
          for (sym <- members if !sym.isConstructor) {
            val sym1 = mkGetter(sym, sym.originalName.decode)
            proxyIntf.info.decls enter sym1
            proxyMap(sym1) = sym
          }
          for (sym <- members if isRefClass(sym.tpe)) {
            val sym1 = mkSetter(sym)
            proxyIntf.info.decls enter sym1
            proxyMap(sym1) = sym
          }
        }
        create(closure, clazz)
      }

      addAccessors
      if (DEBUG) {
        val xs = proxyMap.keysIterator.toList
        println("\tadded "+proxyIntf+
                "\n\twith "+xs.mkString(", ")+" ["+xs.length+"]")
      }
    } //addProxy

    def genProxy(clazz: Symbol) {
      val (proxyIntf, proxyImpl, proxyMap) = proxies(clazz)

      // generate proxy interface
      val ifaceBody = proxyMap.keysIterator.toList map { DefDef(_, EmptyTree) }
      val ifaceParents =
        proxyIntf.info.parents map (t => TypeTree(t) setPos proxyIntf.pos)
      val ifaceTmpl = Template(ifaceParents, emptyValDef, ifaceBody)
      val ifaceDef = localTyper typed ClassDef(proxyIntf, ifaceTmpl)

      // generated proxy implementation
      // Variant 1: rebind/unbind
      val param1 =
        proxyImpl.newValueParameter(proxyImpl.pos, freshName("x$"))
         .setFlag(SYNTHETIC | PARAMACCESSOR | PRIVATE | LOCAL)
         .setInfo(StringClass.tpe)
      proxyImpl.info.decls enter param1

      val param2 =
        proxyImpl.newValueParameter(proxyImpl.pos, freshName("x$"))
          .setFlag(SYNTHETIC | PARAMACCESSOR | PRIVATE | LOCAL)
          .setInfo(clazz.tpe)
      proxyImpl.info.decls enter param2

      val unreferenced =
        proxyImpl.newMethod(proxyImpl.pos, nme_unreferenced)
          .setInfo(MethodType(List(), UnitClass.tpe))
      proxyImpl.info.decls enter unreferenced

      val proxyBody =
        DefDef(unreferenced, List(List()), Block(
          List(Apply( //stats
            Select(gen.mkAttributedRef(DebugModule), "info"),
            List(Apply(
              Select(Literal(Constant("unreferenced: ")), "$plus"),
              // Variant 1: rebind/unbind
              List(Select(This(proxyImpl), param1.name))
              // Variant 2: un-/exportObject
              //List(This(proxyImpl))
            ))
          )),
          Apply( //expr
            Select(gen.mkAttributedRef(RemoteRefModule), nme_unbind),
            // Variant 1: rebind/unbind
            List(Select(This(proxyImpl), param1.name))
            // Variant 2: un-/exportObject
            //List(This(proxyImpl))
          )
        )) :: (
        for (sym <- proxyIntf.info.decls.toList) yield {
          val sym1 = sym.cloneSymbol(proxyImpl)
          sym1 resetFlag (ABSTRACT | DEFERRED | lateDEFERRED)
          proxyImpl.info.decls enter sym1
          DefDef(sym1, {
            val sym2 = proxyMap(sym)
            var t = Select(This(proxyImpl), param2)
            var outerAcc =
              if (sym2.owner isSubClass param2) None
              else param2.info.decls.toList find (_.isOuterAccessor)
            while (!outerAcc.isEmpty) {
              t = Select(t, outerAcc.get)
              val outerClass = outerAcc.get.tpe.resultType.typeSymbol
              outerAcc =
                if (sym2.owner == outerClass) None
                else outerClass.info.decls.toList find (_.isOuterAccessor)
            }
            val sel = Select(t, sym2)
            if (sym2.isMethod) {
              Apply(sel, sym1.paramss(0) map { Ident(_) })
            }
            else if (isRefClass(sym2.tpe)) {
              val sel1 = Select(sel, nme.elem)
              if (sym1.tpe.paramTypes.length == 0) sel1
              else Assign(sel1, Ident(sym1.paramss(0)(0)))
            }
            else
              sel
          })
        })
      val proxyParents =
        proxyImpl.info.parents map (t => TypeTree(t) setPos proxyImpl.pos)
      val proxyTmpl = Template(proxyParents,
                           emptyValDef, NoMods,
              // Variant 1: rebind/unbind
              /*vparamss*/ List(List(ValDef(param1), ValDef(param2))),
              // Variant 2: un-/exportObject
              ///*vparamss*/ List(List(ValDef(param2))),
                 /*argss*/ List(List()), proxyBody, NoPosition)
      val proxyDef = localTyper typed ClassDef(proxyImpl, proxyTmpl)

      // remember definitions to be added by transformStats
      val proxyOwner = proxyIntf.owner
      if (! (proxyInterfaceDefs contains proxyOwner))
        proxyInterfaceDefs(proxyOwner) = new ListBuffer
      proxyInterfaceDefs(proxyOwner) += ifaceDef
      proxyInterfaceDefs(proxyOwner) += proxyDef
    } //genProxy

    private def freshName(s: String): Name =
      unit.fresh.newName(s)

    private def freshProxyName: Name =
      unit.fresh.newName(PROXY_PREFIX)

    /** <p>
     *   Given a detached closure applied in some environment consisting
     *   of an enclosing class <code>C</code> and some local variables
     *   <code>x$1</code> (immutable) and <code>y$1</code> (mutable):
     *  </p><pre>
     *    scala.remoting.detach.apply({
     *      (new $anonfun$1(C.this, x$1, y$1): Function1)
     *    })</pre>
     *  <p>
     *    the above code is transformed to the following block:
     *  </p><pre>
     *    {
     *      val proxy$1: C$Proxy =
     *        RemoteRef.bind("C/proxy$1", new C$ProxyImpl(C.this))
     *      val proxy$2: RemoteIntRef =
     *        RemoteRef.bind("C/proxy$2", new RemoteIntRefImpl(y$1))
     *      (new $anonfun$1detach(proxy$1, x$1, proxy$2): Function1)
     *    }
     *  </pre>
     */
    private def mkClosureApply(tree: Tree): Tree = {
      val apply @ Apply(fun, args) = detachedClosureApply(tree)
      assert(fun.symbol.isConstructor, fun.symbol+" is not a constructor")//debug
      val clazz = apply.tpe.typeSymbol
      val thiz = capturedThisClass(clazz)
      val cdef = mkClosureDef(clazz)
      val uid = localTyper typed {
        val sym = currentOwner.newValue(tree.pos, freshName("uid$"))
          .setFlag(SYNTHETIC)
          .setInfo(StringClass.tpe)
        val rhs = Apply(Select(
          Apply(
            Select(New(TypeTree(UIDClass.tpe)), nme.CONSTRUCTOR),
            List()
          ),
          "toString"
        ), List())
        ValDef(sym, rhs)
      }
      def cast(tree: Tree, tpe: Type): Tree =
        Apply(
          TypeApply(
            Select(tree, Object_asInstanceOf),
            List(TypeTree(tpe))
          ),
          List()
        )

      def mkProxy(csym: Symbol): ValDef = {
        val (iface, proxy, _) = proxies(csym)
        val sym = currentOwner.newValue(csym.pos, freshProxyName)
          .setFlag(SYNTHETIC)
          .setInfo(iface.tpe)
        val bind = Select(gen.mkAttributedRef(RemoteRefModule), nme_bind)
        val name = Apply(
          Select(Literal(Constant(sym.fullName('/')+"$")), String_+),
          List(Ident(uid.symbol))
        )
        val thiz =
          if (csym.isModule) gen.mkAttributedIdent(csym)
          else gen.mkAttributedThis(csym)
        val args = List(name,
                        Apply(Select(New(TypeTree(proxy.tpe)), nme.CONSTRUCTOR),
                              // Variant 1: rebind/unbind
                              List(name, thiz)))
                              // Variant 2: un-/exportObject
                              //List(thiz)))
        val rhs = cast(Apply(bind, args), iface.tpe)
        ValDef(sym, rhs)
      }

      def mkObjProxies: List[ValDef] = {
        val (outer, captured) =
           capturedObjects(clazz).toList partition (thiz.==)
        (captured ::: outer) map mkProxy
      }

      def mkArgProxies: Map[Symbol, ValDef] = {
        def retRefs(t: Tree): List[Tree] = t match {
          case Apply(fun, args) =>
            args flatMap retRefs
          case id @ Ident(_) =>
            if (isRefClass(id.tpe)) List(id) else Nil
          case Template(_, _, body) =>
            body flatMap retRefs
          case New(tpt) =>
            retRefs(tpt)
          case thiz @ This(_) =>
            if (isRefClass(thiz.tpe)) List(thiz) else Nil
          case _ =>
            throw new Error("Internal error: " + t.getClass)
        }
        new immutable.HashMap[Symbol, ValDef] ++ (
          for (variable <- retRefs(apply)) yield {
            val param = variable.symbol
            assert(isRefClass(param.tpe), param)
            val proxy = currentOwner.newValue(param.pos, freshProxyName)
              .setFlag(SYNTHETIC)
              .setInfo(mkRemoteRefClass(param.tpe))
            val bind = Select(gen.mkAttributedRef(RemoteRefModule), nme_bind)
            //val name = Literal(Constant(proxy.fullName('/')))
            val name = Apply(
              Select(Literal(Constant(proxy.fullName('/')+"$")), String_+),
              List(Ident(uid.symbol))
            )
            val ts = param.tpe.typeSymbol
            val args = List(name,
                            Apply(
                              Select(New(TypeTree(remoteRefImpl(ts).tpe)), nme.CONSTRUCTOR),
                              // Variant 1: rebind/unbind
                              List(name, variable)))
                              // Variant 2: un-/exportObject
                              //List(variable)))
            val rhs = cast(Apply(bind, args), remoteRefClass(ts).tpe)
            (param, ValDef(proxy, rhs))
          }
        )
      } //mkArgProxies

      /** <p>
       *   Method <code>mkClosureInstance</code> updates the list of actual
       *   parameters passed to the closure instance.
       *  </p>
       */
      def mkClosureInstance(objProxies: List[ValDef],
                            argProxies: Map[Symbol, ValDef]): Tree = {
        fun.tpe = fun.symbol.tpe
        val args0 = objProxies map (tree => Ident(tree.symbol))
        val hasOuter = !args.isEmpty && (args.head.symbol.tpe == thiz.tpe)
        val args1 = (if (hasOuter) args.tail else args) map (arg =>
          argProxies get arg.symbol match {
            case Some(t) => Ident(t.symbol)
            case None => arg
          }
        )
        if (DEBUG)
          println("\nmkClosureInstance:\n\targs0="+args0+"\n\targs1="+args1)
        val t = Typed(
                  Apply(fun, args0 ::: args1),
                  //TypeTree(clazz.info.parents.tail.head) //interface (2.7.x)
                  TypeTree(clazz.info.parents.head) //interface (2.8.x)
                )
        localTyper typed t
      } //mkClosureInstance

      val objProxies = mkObjProxies
      val argProxies = mkArgProxies
      val stats = uid :: objProxies ::: argProxies.valuesIterator.toList
      val expr = mkClosureInstance(objProxies, argProxies)
      localTyper typed Block(stats, expr)
    } //mkClosureApply

    override def transform(tree: Tree): Tree = {
      def withInConstructorFlag(inConstructorFlag: Long)(f: => Tree): Tree = {
        val savedInConstructorFlag = this.inConstructorFlag
        this.inConstructorFlag = inConstructorFlag
        val t = f
        this.inConstructorFlag = savedInConstructorFlag
        t
      }
      if (!isEnabled) return tree
      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          val tree1 = super.transform(tree)
          if (!reporter.hasErrors && (capturedThisClass contains tree1.symbol))
            mkClosureDef(tree1.symbol)
          else
            tree1

        case Apply(Select(_, _), _) =>
          val tree1 = super.transform(tree)
          if (!reporter.hasErrors && (detachedClosureApply contains tree1))
            atPos(tree1.pos)(mkClosureApply(tree1))
          else
            tree1

        case Template(_, _, _) =>
          withInConstructorFlag(0) { super.transform(tree) }

        case _ =>
          super.transform(tree)
      }
    }

    /** Transform statements and add detached definitions to them. */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val stats1 = super.transformStats(stats, exprOwner)
      val newDefs = {
        val buf = new ListBuffer[Tree]
        if (proxyInterfaceDefs contains currentOwner)
          buf ++= proxyInterfaceDefs(currentOwner).toList
        buf.toList
      }
      if (newDefs.isEmpty) stats1 else stats1 ::: newDefs
    }

    private def genProxies() {
      def printDebugInfo() {
        println("\ncompilation unit : "+unit)
        for ((sym, _) <- detachedClosure) {
          println("closure to detach: "+sym+" (owner: "+sym.owner+")")
          println("captured this    : "+capturedThisClass(sym))
          val objs = capturedObjects get sym match {
            case Some(ss) => ss.toList
            case None => Nil
          }
          println("captured objects : "+objs.mkString(", ")+" ["+objs.length+"]")
        }
        println("\ncalled functions :")
        for (sym <- capturedFuncs.keysIterator) {
          val xs = capturedFuncs(sym).toList map (s => {
            val callers = capturedCallers get s match {
              case Some(ss) => "|"+ss.toList.mkString(",")
              case None => ""
            }
            s+"("+s.owner.name+callers+")"
          })
          println("\t"+sym+" -> "+xs.mkString(", ")+" ["+xs.length+"]")
        }
      }
      def printDebugInfo2() {
        println("\nproxy classes    :")
        for (sym <- proxies.keysIterator)
          println("\t"+sym+"("+sym.tpe+") -> "+proxies(sym))
      }
      if (DEBUG)
        printDebugInfo
      for ((closure, _) <- detachedClosure;
           captured <- capturedObjects(closure))
        addProxy(closure, captured)
      if (DEBUG)
        printDebugInfo2
      for (sym <- proxies.keysIterator)
        genProxy(sym)
    } //genProxies

    /** <p>
     *    Method <code>transformUnit</code> performs three successive operations:
     *  </p>
     *  <ol>
     *    <li>it first gathers infos about free objects and detached
     *      closures;</li>
     *    <li>it then adds proxies for free objects;</li>
     *    <li>finally, if transforms detached closures (both definition and
     *       instantiation).</li>
     *  </ol>
     */
    override def transformUnit(unit: CompilationUnit) {
      freeObjTraverser.traverse(unit.body)
      if (!reporter.hasErrors) genProxies
      super.transformUnit(unit)
    }
  }

}

