/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc.transform

import scala.collection.mutable.ListBuffer
import symtab.Flags._
import util.TreeSet

/** This phase converts classes with parameters into Java-like classes with
 *  fields, which are assigned to from constructors.
 */
abstract class Constructors extends Transform with ast.TreeDSL {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "constructors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ConstructorTransformer(unit)

  class ConstructorTransformer(unit: CompilationUnit) extends Transformer {

    def transformClassTemplate(impl: Template): Template = {
      val clazz = impl.symbol.owner  // the transformed class
      val stats = impl.body          // the transformed template body
      val localTyper = typer.atOwner(impl, clazz)

      var constr: DefDef = null      // The primary constructor
      var constrParams: List[Symbol] = null // ... and its parameters
      var constrBody: Block = null   // ... and its body

      // decompose primary constructor into the three entities above.
      for (stat <- stats) {
        stat match {
          case ddef @ DefDef(_, _, _, List(vparams), _, rhs @ Block(_, Literal(_))) =>
            if (ddef.symbol.isPrimaryConstructor) {
              constr = ddef
              constrParams = vparams map (_.symbol)
              constrBody = rhs
            }
          case _ =>
        }
      }
      assert((constr ne null) && (constrBody ne null), impl)

      // The parameter accessor fields which are members of the class
      val paramAccessors = clazz.constrParamAccessors

      // The constructor parameter corresponding to an accessor
      def parameter(acc: Symbol): Symbol =
        parameterNamed(nme.getterName(acc.originalName))

      // The constructor parameter with given name. This means the parameter
      // has given name, or starts with given name, and continues with a `$' afterwards.
      def parameterNamed(name: Name): Symbol = {
        def matchesName(param: Symbol) =
          param.name == name ||
          param.name.startsWith(name) && param.name(name.length) == '$'
        val ps = constrParams filter matchesName
        if (ps.isEmpty) assert(false, "" + name + " not in " + constrParams)
        ps.head
      }

      var thisRefSeen: Boolean = false

      // A transformer for expressions that go into the constructor
      val intoConstructorTransformer = new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(This(_), _), List()) =>
            // references to parameter accessor methods of own class become references to parameters
            // outer accessors become references to $outer parameter
            if ((tree.symbol hasFlag PARAMACCESSOR) && tree.symbol.owner == clazz)
              gen.mkAttributedIdent(parameter(tree.symbol.accessed)) setPos tree.pos
            else if (tree.symbol.outerSource == clazz && !clazz.isImplClass)
              gen.mkAttributedIdent(parameterNamed(nme.OUTER)) setPos tree.pos
            else
              super.transform(tree)
          case Select(This(_), _)
          if ((tree.symbol hasFlag PARAMACCESSOR) && !tree.symbol.isSetter && tree.symbol.owner == clazz) =>
            // references to parameter accessor field of own class become references to parameters
            gen.mkAttributedIdent(parameter(tree.symbol)) setPos tree.pos
          case Select(_, _) =>
            thisRefSeen = true
            super.transform(tree)
          case This(_) =>
            thisRefSeen = true
            super.transform(tree)
          case Super(_, _) =>
            thisRefSeen = true
            super.transform(tree)
          case _ =>
            super.transform(tree)
        }
      }

      // Move tree into constructor, take care of changing owner from `oldowner' to constructor symbol
      def intoConstructor(oldowner: Symbol, tree: Tree) =
        intoConstructorTransformer.transform(
          new ChangeOwnerTraverser(oldowner, constr.symbol)(tree))

      // Should tree be moved in front of super constructor call?
      def canBeMoved(tree: Tree) = tree match {
        //todo: eliminate thisRefSeen
        case ValDef(mods, _, _, _) =>
          if (settings.Xwarninit.value)
            if (!(mods hasFlag PRESUPER | PARAMACCESSOR) && !thisRefSeen &&
                { val g = tree.symbol.getter(tree.symbol.owner);
                 g != NoSymbol && !g.allOverriddenSymbols.isEmpty
               })
              unit.warning(tree.pos, "the semantics of this definition has changed;\nthe initialization is no longer be executed before the superclass is called")
          (mods hasFlag PRESUPER | PARAMACCESSOR)// || !thisRefSeen && (!settings.future.value && !settings.checkInit.value)
        case _ => false
      }

      // Create an assignment to class field `to' with rhs `from'
      def mkAssign(to: Symbol, from: Tree): Tree =
        localTyper.typed {
          //util.trace("compiling "+unit+" ") {
            atPos(to.pos) {
              Assign(Select(This(clazz), to), from)
            }
          //}
        }

      // Create code to copy parameter to parameter accessor field.
      // If parameter is $outer, check that it is not null.
      def copyParam(to: Symbol, from: Symbol): Tree = {
        import CODE._
        var result = mkAssign(to, Ident(from))
        if (from.name == nme.OUTER)
          result =
            atPos(to.pos) {
              localTyper.typed {
                IF (from EQREF NULL) THEN THROW(NullPointerExceptionClass) ELSE result
              }
            }

        result
      }

      // The list of definitions that go into class
      val defBuf = new ListBuffer[Tree]

      // The list of statements that go into constructor after superclass constructor call
      val constrStatBuf = new ListBuffer[Tree]

      // The list of statements that go into constructor before superclass constructor call
      val constrPrefixBuf = new ListBuffer[Tree]

      // The early initialized field definitions of the class (these are the class members)
      val presupers = treeInfo.preSuperFields(stats)

      // generate code to copy pre-initialized fields
      for (stat <- constrBody.stats) {
        constrStatBuf += stat
        stat match {
          case ValDef(mods, name, _, _) if (mods hasFlag PRESUPER) =>
            // stat is the constructor-local definition of the field value
            val fields = presupers filter (
              vdef => nme.localToGetter(vdef.name) == name)
            assert(fields.length == 1)
            constrStatBuf += mkAssign(fields.head.symbol, Ident(stat.symbol))
          case _ =>
        }
      }

      // Triage all template definitions to go into defBuf, constrStatBuf, or constrPrefixBuf.
      for (stat <- stats) stat match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          // methods with constant result type get literals as their body
          // all methods except the primary constructor go into template
          stat.symbol.tpe match {
            case MethodType(List(), tp @ ConstantType(c)) =>
              defBuf += treeCopy.DefDef(
                stat, mods, name, tparams, vparamss, tpt,
                Literal(c) setPos rhs.pos setType tp)
            case _ =>
              if (!stat.symbol.isPrimaryConstructor) defBuf += stat
          }
        case ValDef(mods, name, tpt, rhs) =>
          // val defs with constant right-hand sides are eliminated.
          // for all other val defs, an empty valdef goes into the template and
          // the initializer goes as an assignment into the constructor
          // if the val def is an early initialized or a parameter accessor, it goes
          // before the superclass constructor call, otherwise it goes after.
          // Lazy vals don't get the assignment in the constructor.
          if (!stat.symbol.tpe.isInstanceOf[ConstantType]) {
            if (rhs != EmptyTree && !stat.symbol.hasFlag(LAZY)) {
              val rhs1 = intoConstructor(stat.symbol, rhs);
              (if (canBeMoved(stat)) constrPrefixBuf else constrStatBuf) += mkAssign(
                stat.symbol, rhs1)
            }
            defBuf += treeCopy.ValDef(stat, mods, name, tpt, EmptyTree)
          }
        case ClassDef(_, _, _, _) =>
          // classes are treated recursively, and left in the template
          defBuf += new ConstructorTransformer(unit).transform(stat)
        case _ =>
          // all other statements go into the constructor
          constrStatBuf += intoConstructor(impl.symbol, stat)
      }

      // ----------- avoid making fields for symbols that are not accessed --------------

      // A sorted set of symbols that are known to be accessed outside the primary constructor.
      val accessedSyms = new TreeSet[Symbol]((x, y) => x isLess y)

      // a list of outer accessor symbols and their bodies
      var outerAccessors: List[(Symbol, Tree)] = List()

      // Could symbol's definition be omitted, provided it is not accessed?
      // This is the case if the symbol is defined in the current class, and
      // ( the symbol is an object private parameter accessor field, or
      //   the symbol is an outer accessor of a final class which does not override another outer accesser. )
      def maybeOmittable(sym: Symbol) =
        (sym.owner == clazz &&
         ((sym hasFlag PARAMACCESSOR) && sym.isPrivateLocal ||
          sym.isOuterAccessor && sym.owner.isFinal && sym.allOverriddenSymbols.isEmpty))

      // Is symbol known to be accessed outside of the primary constructor,
      // or is it a symbol whose definition cannot be omitted anyway?
      def mustbeKept(sym: Symbol) =
        !maybeOmittable(sym) || (accessedSyms contains sym)

      // A traverser to set accessedSyms and outerAccessors
      val accessTraverser = new Traverser {
        override def traverse(tree: Tree) = {
          tree match {
            case DefDef(_, _, _, _, _, body)
            if (tree.symbol.isOuterAccessor && tree.symbol.owner == clazz && clazz.isFinal) =>
              outerAccessors ::= (tree.symbol, body)
            case Select(_, _) =>
              if (!mustbeKept(tree.symbol)) accessedSyms addEntry tree.symbol
              super.traverse(tree)
            case _ =>
              super.traverse(tree)
          }
        }
      }

      // first traverse all definitions except outeraccesors
      // (outeraccessors are avoided in accessTraverser)
      for (stat <- defBuf.iterator) accessTraverser.traverse(stat)

      // then traverse all bodies of outeraccessors which are accessed themselves
      // note: this relies on the fact that an outer accessor never calls another
      // outer accessor in the same class.
      for ((accSym, accBody) <- outerAccessors)
        if (mustbeKept(accSym)) accessTraverser.traverse(accBody)

      // Conflicting symbol list from parents: see bug #1960.
      // It would be better to mangle the constructor parameter name since
      // it can only be used internally, but I think we need more robust name
      // mangling before we introduce more of it.
      val parentSymbols = Map((for {
        p <- impl.parents
        if p.symbol.isTrait
        sym <- p.symbol.info.nonPrivateMembers
        if sym.isGetter && !sym.isOuterField
      } yield sym.name -> p): _*)

      // Initialize all parameters fields that must be kept.
      val paramInits =
        for (acc <- paramAccessors if mustbeKept(acc)) yield {
          if (parentSymbols contains acc.name)
            unit.error(acc.pos, "parameter '%s' requires field but conflicts with %s in '%s'".format(
              acc.name, acc.name, parentSymbols(acc.name)))

          copyParam(acc, parameter(acc))
        }

      // Assemble final constructor
      defBuf += treeCopy.DefDef(
        constr, constr.mods, constr.name, constr.tparams, constr.vparamss, constr.tpt,
        treeCopy.Block(
          constrBody,
          paramInits ::: constrPrefixBuf.toList ::: constrStatBuf.toList,
          constrBody.expr));

      // Unlink all fields that can be dropped from class scope
      for (sym <- clazz.info.decls.toList)
        if (!mustbeKept(sym)) clazz.info.decls unlink sym

      // Eliminate all field definitions that can be dropped from template
      treeCopy.Template(impl, impl.parents, impl.self,
        defBuf.toList filter (stat => mustbeKept(stat.symbol)))
    } // transformClassTemplate

    override def transform(tree: Tree): Tree =
      tree match {
        case ClassDef(mods, name, tparams, impl) if !tree.symbol.hasFlag(INTERFACE) =>
          treeCopy.ClassDef(tree, mods, name, tparams, transformClassTemplate(impl))
        case _ =>
          super.transform(tree)
      }
  } // ConstructorTransformer
}
