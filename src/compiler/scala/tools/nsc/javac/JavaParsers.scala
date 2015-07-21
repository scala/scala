/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
//todo: allow infix type patterns


package scala.tools.nsc
package javac

import scala.collection.mutable.ListBuffer
import symtab.Flags
import JavaTokens._
import scala.language.implicitConversions
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.ListOfNil

trait JavaParsers extends ast.parser.ParsersCommon with JavaScanners {
  val global : Global
  import global._
  import definitions._

  case class JavaOpInfo(operand: Tree, operator: Name, pos: Int)

  class JavaUnitParser(val unit: global.CompilationUnit) extends JavaParser {
    val in = new JavaUnitScanner(unit)
    def freshName(prefix: String): Name = freshTermName(prefix)
    def freshTermName(prefix: String): TermName = unit.freshTermName(prefix)
    def freshTypeName(prefix: String): TypeName = unit.freshTypeName(prefix)
    def deprecationWarning(off: Int, msg: String) = currentRun.reporting.deprecationWarning(off, msg)
    implicit def i2p(offset : Int) : Position = Position.offset(unit.source, offset)
    def warning(pos : Int, msg : String) : Unit = reporter.warning(pos, msg)
    def syntaxError(pos: Int, msg: String) : Unit = reporter.error(pos, msg)
  }

  abstract class JavaParser extends ParserCommon {
    val in: JavaScanner

    def freshName(prefix : String): Name
    protected implicit def i2p(offset : Int) : Position
    private implicit def p2i(pos : Position): Int = if (pos.isDefined) pos.point else -1

    /** The simple name of the package of the currently parsed file */
    private var thisPackageName: TypeName = tpnme.EMPTY

    /** this is the general parse method
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

    // -------- error handling ---------------------------------------

    private var lastErrorPos : Int = -1

    protected def skip() {
      var nparens = 0
      var nbraces = 0
      while (true) {
        in.token match {
          case EOF =>
            return
          case SEMI =>
            if (nparens == 0 && nbraces == 0) return
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            if (nbraces == 0) return
            nbraces -= 1
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        in.nextToken()
      }
    }

    def warning(pos : Int, msg : String) : Unit
    def syntaxError(pos: Int, msg: String) : Unit
    def syntaxError(msg: String, skipIt: Boolean) {
      syntaxError(in.currentPos, msg, skipIt)
    }

    def syntaxError(pos: Int, msg: String, skipIt: Boolean) {
      if (pos > lastErrorPos) {
        syntaxError(pos, msg)
        // no more errors on this token.
        lastErrorPos = in.currentPos
      }
      if (skipIt)
        skip()
    }
    def errorTypeTree = TypeTree().setType(ErrorType) setPos in.currentPos

    // --------- tree building -----------------------------

    import gen.{ rootId, scalaDot }

    def javaDot(name: Name): Tree =
      Select(rootId(nme.java), name)

    def javaLangDot(name: Name): Tree =
      Select(javaDot(nme.lang), name)

    def javaLangObject(): Tree = javaLangDot(tpnme.Object)

    def arrayOf(tpt: Tree) =
      AppliedTypeTree(scalaDot(tpnme.Array), List(tpt))

    def blankExpr = Ident(nme.WILDCARD)

    def makePackaging(pkg: RefTree, stats: List[Tree]): PackageDef =
      atPos(pkg.pos) {  PackageDef(pkg, stats) }

    def makeTemplate(parents: List[Tree], stats: List[Tree]) =
      Template(
        parents,
        noSelfType,
        if (treeInfo.firstConstructor(stats) == EmptyTree) makeConstructor(List()) :: stats
        else stats)

    def makeSyntheticParam(count: Int, tpt: Tree): ValDef =
      makeParam(nme.syntheticParamName(count), tpt)
    def makeParam(name: String, tpt: Tree): ValDef =
      makeParam(TermName(name), tpt)
    def makeParam(name: TermName, tpt: Tree): ValDef =
      ValDef(Modifiers(Flags.JAVA | Flags.PARAM), name, tpt, EmptyTree)

    def makeConstructor(formals: List[Tree]) = {
      val vparams = mapWithIndex(formals)((p, i) => makeSyntheticParam(i + 1, p))
      DefDef(Modifiers(Flags.JAVA), nme.CONSTRUCTOR, List(), List(vparams), TypeTree(), blankExpr)
    }

    // ------------- general parsing ---------------------------

    /** skip parent or brace enclosed sequence of things */
    def skipAhead() {
      var nparens = 0
      var nbraces = 0
      do {
        in.token match {
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        in.nextToken()
        in.token match {
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            nbraces -= 1
          case _ =>
        }
      } while (in.token != EOF && (nparens > 0 || nbraces > 0))
    }

    def skipTo(tokens: Int*) {
      while (!(tokens contains in.token) && in.token != EOF) {
        if (in.token == LBRACE) { skipAhead(); accept(RBRACE) }
        else if (in.token == LPAREN) { skipAhead(); accept(RPAREN) }
        else in.nextToken()
      }
    }

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      */
    def accept(token: Int): Int = {
      val pos = in.currentPos
      if (in.token != token) {
        val posToReport = in.currentPos
        val msg =
          JavaScannerConfiguration.token2string(token) + " expected but " +
            JavaScannerConfiguration.token2string(in.token) + " found."

        syntaxError(posToReport, msg, skipIt = true)
      }
      if (in.token == token) in.nextToken()
      pos
    }

    def acceptClosingAngle() {
      val closers: PartialFunction[Int, Int] = {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => EQUALS
      }
      if (closers isDefinedAt in.token) in.token = closers(in.token)
      else accept(GT)
    }

    def identForType(): TypeName = ident().toTypeName
    def ident(): Name =
      if (in.token == IDENTIFIER) {
        val name = in.name
        in.nextToken()
        name
      } else {
        accept(IDENTIFIER)
        nme.ERROR
      }

    def repsep[T <: Tree](p: () => T, sep: Int): List[T] = {
      val buf = ListBuffer[T](p())
      while (in.token == sep) {
        in.nextToken()
        buf += p()
      }
      buf.toList
    }

    /** Convert (qual)ident to type identifier
     */
    def convertToTypeId(tree: Tree): Tree = gen.convertToTypeName(tree) match {
      case Some(t)  => t setPos tree.pos
      case _        => tree match {
        case AppliedTypeTree(_, _) | ExistentialTypeTree(_, _) | SelectFromTypeTree(_, _) =>
          tree
        case _ =>
          syntaxError(tree.pos, "identifier expected", skipIt = false)
          errorTypeTree
      }
    }

    // -------------------- specific parsing routines ------------------

    def qualId(): RefTree = {
      var t: RefTree = atPos(in.currentPos) { Ident(ident()) }
      while (in.token == DOT) {
        in.nextToken()
        t = atPos(in.currentPos) { Select(t, ident()) }
      }
      t
    }

    def optArrayBrackets(tpt: Tree): Tree =
      if (in.token == LBRACKET) {
        val tpt1 = atPos(in.pos) { arrayOf(tpt) }
        in.nextToken()
        accept(RBRACKET)
        optArrayBrackets(tpt1)
      } else tpt

    def basicType(): Tree =
      atPos(in.pos) {
        in.token match {
          case BYTE    => in.nextToken(); TypeTree(ByteTpe)
          case SHORT   => in.nextToken(); TypeTree(ShortTpe)
          case CHAR    => in.nextToken(); TypeTree(CharTpe)
          case INT     => in.nextToken(); TypeTree(IntTpe)
          case LONG    => in.nextToken(); TypeTree(LongTpe)
          case FLOAT   => in.nextToken(); TypeTree(FloatTpe)
          case DOUBLE  => in.nextToken(); TypeTree(DoubleTpe)
          case BOOLEAN => in.nextToken(); TypeTree(BooleanTpe)
          case _       => syntaxError("illegal start of type", skipIt = true); errorTypeTree
        }
      }

    def typ(): Tree =
      optArrayBrackets {
        if (in.token == FINAL) in.nextToken()
        if (in.token == IDENTIFIER) {
          var t = typeArgs(atPos(in.currentPos)(Ident(ident())))
          // typeSelect generates Select nodes is the lhs is an Ident or Select,
          // SelectFromTypeTree otherwise. See #3567.
          // Select nodes can be later
          // converted in the typechecker to SelectFromTypeTree if the class
          // turns out to be an instance ionner class instead of a static inner class.
          def typeSelect(t: Tree, name: Name) = t match {
            case Ident(_) | Select(_, _) => Select(t, name)
            case _ => SelectFromTypeTree(t, name.toTypeName)
          }
          while (in.token == DOT) {
            in.nextToken()
            t = typeArgs(atPos(in.currentPos)(typeSelect(t, ident())))
          }
          convertToTypeId(t)
        } else {
          basicType()
        }
      }

    def typeArgs(t: Tree): Tree = {
      val wildcards = new ListBuffer[TypeDef]
      def typeArg(): Tree =
        if (in.token == QMARK) {
          val pos = in.currentPos
          in.nextToken()
          val hi = if (in.token == EXTENDS) { in.nextToken() ; typ() } else EmptyTree
          val lo = if (in.token == SUPER)   { in.nextToken() ; typ() } else EmptyTree
          val tdef = atPos(pos) {
            TypeDef(
              Modifiers(Flags.JAVA | Flags.DEFERRED),
              newTypeName("_$"+ (wildcards.length + 1)),
              List(),
              TypeBoundsTree(lo, hi))
          }
          wildcards += tdef
          atPos(pos) { Ident(tdef.name) }
        } else {
          typ()
        }
      if (in.token == LT) {
        in.nextToken()
        val t1 = convertToTypeId(t)
        val args = repsep(typeArg, COMMA)
        acceptClosingAngle()
        atPos(t1.pos) {
          val t2: Tree = AppliedTypeTree(t1, args)
          if (wildcards.isEmpty) t2
          else ExistentialTypeTree(t2, wildcards.toList)
        }
      } else t
    }

    def annotations(): List[Tree] = {
      //var annots = new ListBuffer[Tree]
      while (in.token == AT) {
        in.nextToken()
        annotation()
      }
      List() // don't pass on annotations for now
    }

    /** Annotation ::= TypeName [`(` AnnotationArgument {`,` AnnotationArgument} `)`]
     */
    def annotation() {
      qualId()
      if (in.token == LPAREN) { skipAhead(); accept(RPAREN) }
      else if (in.token == LBRACE) { skipAhead(); accept(RBRACE) }
    }

    def modifiers(inInterface: Boolean): Modifiers = {
      var flags: Long = Flags.JAVA
      // assumed true unless we see public/private/protected
      var isPackageAccess = true
      var annots: List[Tree] = Nil
      def addAnnot(sym: Symbol) = annots :+= New(sym.tpe)

      while (true) {
        in.token match {
          case AT if (in.lookaheadToken != INTERFACE) =>
            in.nextToken()
            annotation()
          case PUBLIC =>
            isPackageAccess = false
            in.nextToken()
          case PROTECTED =>
            flags |= Flags.PROTECTED
            in.nextToken()
          case PRIVATE =>
            isPackageAccess = false
            flags |= Flags.PRIVATE
            in.nextToken()
          case STATIC =>
            flags |= Flags.STATIC
            in.nextToken()
          case ABSTRACT =>
            flags |= Flags.ABSTRACT
            in.nextToken()
          case FINAL =>
            flags |= Flags.FINAL
            in.nextToken()
          case DEFAULT =>
            flags |= Flags.JAVA_DEFAULTMETHOD
            in.nextToken()
          case NATIVE =>
            addAnnot(NativeAttr)
            in.nextToken()
          case TRANSIENT =>
            addAnnot(TransientAttr)
            in.nextToken()
          case VOLATILE =>
            addAnnot(VolatileAttr)
            in.nextToken()
          case SYNCHRONIZED | STRICTFP =>
            in.nextToken()
          case _ =>
            val privateWithin: TypeName =
              if (isPackageAccess && !inInterface) thisPackageName
              else tpnme.EMPTY

            return Modifiers(flags, privateWithin) withAnnotations annots
        }
      }
      abort("should not be here")
    }

    def typeParams(): List[TypeDef] =
      if (in.token == LT) {
        in.nextToken()
        val tparams = repsep(typeParam, COMMA)
        acceptClosingAngle()
        tparams
      } else List()

    def typeParam(): TypeDef =
      atPos(in.currentPos) {
        val name = identForType()
        val hi = if (in.token == EXTENDS) { in.nextToken() ; bound() } else EmptyTree
        TypeDef(Modifiers(Flags.JAVA | Flags.DEFERRED | Flags.PARAM), name, Nil, TypeBoundsTree(EmptyTree, hi))
      }

    def bound(): Tree =
      atPos(in.currentPos) {
        val buf = ListBuffer[Tree](typ())
        while (in.token == AMP) {
          in.nextToken()
          buf += typ()
        }
        val ts = buf.toList
        if (ts.tail.isEmpty) ts.head
        else CompoundTypeTree(Template(ts, noSelfType, List()))
      }

    def formalParams(): List[ValDef] = {
      accept(LPAREN)
      val vparams = if (in.token == RPAREN) List() else repsep(formalParam, COMMA)
      accept(RPAREN)
      vparams
    }

    def formalParam(): ValDef = {
      if (in.token == FINAL) in.nextToken()
      annotations()
      var t = typ()
      if (in.token == DOTDOTDOT) {
        in.nextToken()
        t = atPos(t.pos) {
          AppliedTypeTree(scalaDot(tpnme.JAVA_REPEATED_PARAM_CLASS_NAME), List(t))
        }
      }
     varDecl(in.currentPos, Modifiers(Flags.JAVA | Flags.PARAM), t, ident().toTermName)
    }

    def optThrows() {
      if (in.token == THROWS) {
        in.nextToken()
        repsep(typ, COMMA)
      }
    }

    def methodBody(): Tree = {
      skipAhead()
      accept(RBRACE) // skip block
      blankExpr
    }

    def definesInterface(token: Int) = token == INTERFACE || token == AT

    def termDecl(mods: Modifiers, parentToken: Int): List[Tree] = {
      val inInterface = definesInterface(parentToken)
      val tparams = if (in.token == LT) typeParams() else List()
      val isVoid = in.token == VOID
      var rtpt =
        if (isVoid) {
          in.nextToken()
          TypeTree(UnitTpe) setPos in.pos
        } else typ()
      var pos = in.currentPos
      val rtptName = rtpt match {
        case Ident(name) => name
        case _ => nme.EMPTY
      }
      if (in.token == LPAREN && rtptName != nme.EMPTY && !inInterface) {
        // constructor declaration
        val vparams = formalParams()
        optThrows()
        List {
          atPos(pos) {
            DefDef(mods, nme.CONSTRUCTOR, tparams, List(vparams), TypeTree(), methodBody())
          }
        }
      } else {
        var mods1 = mods
        if (mods hasFlag Flags.ABSTRACT) mods1 = mods &~ Flags.ABSTRACT | Flags.DEFERRED
        pos = in.currentPos
        val name = ident()
        if (in.token == LPAREN) {
          // method declaration
          val vparams = formalParams()
          if (!isVoid) rtpt = optArrayBrackets(rtpt)
          optThrows()
          val isConcreteInterfaceMethod = !inInterface || (mods hasFlag Flags.JAVA_DEFAULTMETHOD) || (mods hasFlag Flags.STATIC)
          val bodyOk = !(mods1 hasFlag Flags.DEFERRED) && isConcreteInterfaceMethod
          val body =
            if (bodyOk && in.token == LBRACE) {
              methodBody()
            } else {
              if (parentToken == AT && in.token == DEFAULT) {
                val annot =
                  atPos(pos) {
                    New(Select(scalaDot(nme.runtime), tpnme.AnnotationDefaultATTR), Nil)
                  }
                mods1 = mods1 withAnnotations annot :: Nil
                skipTo(SEMI)
                accept(SEMI)
                blankExpr
              } else {
                accept(SEMI)
                EmptyTree
              }
            }
          // for abstract methods (of classes), the `DEFERRED` flag is alredy set.
          // here we also set it for interface methods that are not static and not default.
          if (!isConcreteInterfaceMethod) mods1 |= Flags.DEFERRED
          List {
            atPos(pos) {
              DefDef(mods1, name.toTermName, tparams, List(vparams), rtpt, body)
            }
          }
        } else {
          if (inInterface) mods1 |= Flags.FINAL | Flags.STATIC
          val result = fieldDecls(pos, mods1, rtpt, name)
          accept(SEMI)
          result
        }
      }
    }

    /** Parse a sequence of field declarations, separated by commas.
     *  This one is tricky because a comma might also appear in an
     *  initializer. Since we don't parse initializers we don't know
     *  what the comma signifies.
     *  We solve this with a second list buffer `maybe` which contains
     *  potential variable definitions.
     *  Once we have reached the end of the statement, we know whether
     *  these potential definitions are real or not.
     */
    def fieldDecls(pos: Position, mods: Modifiers, tpt: Tree, name: Name): List[Tree] = {
      val buf = ListBuffer[Tree](varDecl(pos, mods, tpt, name.toTermName))
      val maybe = new ListBuffer[Tree] // potential variable definitions.
      while (in.token == COMMA) {
        in.nextToken()
        if (in.token == IDENTIFIER) { // if there's an ident after the comma ...
          val name = ident()
          if (in.token == EQUALS || in.token == SEMI) { // ... followed by a `=` or `;`, we know it's a real variable definition
            buf ++= maybe
            buf += varDecl(in.currentPos, mods, tpt.duplicate, name.toTermName)
            maybe.clear()
          } else if (in.token == COMMA) { // ... if there's a comma after the ident, it could be a real vardef or not.
            maybe += varDecl(in.currentPos, mods, tpt.duplicate, name.toTermName)
          } else { // ... if there's something else we were still in the initializer of the
                   // previous var def; skip to next comma or semicolon.
            skipTo(COMMA, SEMI)
            maybe.clear()
          }
        } else { // ... if there's no ident following the comma we were still in the initializer of the
                 // previous var def; skip to next comma or semicolon.
          skipTo(COMMA, SEMI)
          maybe.clear()
        }
      }
      if (in.token == SEMI) {
        buf ++= maybe // every potential vardef that survived until here is real.
      }
      buf.toList
    }

    def varDecl(pos: Position, mods: Modifiers, tpt: Tree, name: TermName): ValDef = {
      val tpt1 = optArrayBrackets(tpt)
      if (in.token == EQUALS && !mods.isParameter) skipTo(COMMA, SEMI)
      val mods1 = if (mods.isFinal) mods &~ Flags.FINAL else mods | Flags.MUTABLE
      atPos(pos) {
        ValDef(mods1, name, tpt1, blankExpr)
      }
    }

    def memberDecl(mods: Modifiers, parentToken: Int): List[Tree] = in.token match {
      case CLASS | ENUM | INTERFACE | AT =>
        typeDecl(if (definesInterface(parentToken)) mods | Flags.STATIC else mods)
      case _ =>
        termDecl(mods, parentToken)
    }

    def makeCompanionObject(cdef: ClassDef, statics: List[Tree]): Tree =
      atPos(cdef.pos) {
        ModuleDef(cdef.mods & (Flags.AccessFlags | Flags.JAVA), cdef.name.toTermName,
                  makeTemplate(List(), statics))
      }

    def importCompanionObject(cdef: ClassDef): Tree =
      atPos(cdef.pos) {
        Import(Ident(cdef.name.toTermName), ImportSelector.wildList)
      }

    // Importing the companion object members cannot be done uncritically: see
    // ticket #2377 wherein a class contains two static inner classes, each of which
    // has a static inner class called "Builder" - this results in an ambiguity error
    // when each performs the import in the enclosing class's scope.
    //
    // To address this I moved the import Companion._ inside the class, as the first
    // statement.  This should work without compromising the enclosing scope, but may (?)
    // end up suffering from the same issues it does in scala - specifically that this
    // leaves auxiliary constructors unable to access members of the companion object
    // as unqualified identifiers.
    def addCompanionObject(statics: List[Tree], cdef: ClassDef): List[Tree] = {
      def implWithImport(importStmt: Tree) = deriveTemplate(cdef.impl)(importStmt :: _)
      // if there are no statics we can use the original cdef, but we always
      // create the companion so import A._ is not an error (see ticket #1700)
      val cdefNew =
        if (statics.isEmpty) cdef
        else deriveClassDef(cdef)(_ => implWithImport(importCompanionObject(cdef)))

      List(makeCompanionObject(cdefNew, statics), cdefNew)
    }

    def importDecl(): List[Tree] = {
      accept(IMPORT)
      val pos = in.currentPos
      val buf = new ListBuffer[Name]
      def collectIdents() : Int = {
        if (in.token == ASTERISK) {
          val starOffset = in.pos
          in.nextToken()
          buf += nme.WILDCARD
          starOffset
        } else {
          val nameOffset = in.pos
          buf += ident()
          if (in.token == DOT) {
            in.nextToken()
            collectIdents()
          } else nameOffset
        }
      }
      if (in.token == STATIC) in.nextToken()
      else buf += nme.ROOTPKG
      val lastnameOffset = collectIdents()
      accept(SEMI)
      val names = buf.toList
      if (names.length < 2) {
        syntaxError(pos, "illegal import", skipIt = false)
        List()
      } else {
        val qual = ((Ident(names.head): Tree) /: names.tail.init) (Select(_, _))
        val lastname = names.last
        val selector = lastname match {
          case nme.WILDCARD => ImportSelector(lastname, lastnameOffset, null, -1)
          case _            => ImportSelector(lastname, lastnameOffset, lastname, lastnameOffset)
        }
        List(atPos(pos)(Import(qual, List(selector))))
      }
    }

    def interfacesOpt() =
      if (in.token == IMPLEMENTS) {
        in.nextToken()
        repsep(typ, COMMA)
      } else {
        List()
      }

    def classDecl(mods: Modifiers): List[Tree] = {
      accept(CLASS)
      val pos = in.currentPos
      val name = identForType()
      val tparams = typeParams()
      val superclass =
        if (in.token == EXTENDS) {
          in.nextToken()
          typ()
        } else {
          javaLangObject()
        }
      val interfaces = interfacesOpt()
      val (statics, body) = typeBody(CLASS, name)
      addCompanionObject(statics, atPos(pos) {
        ClassDef(mods, name, tparams, makeTemplate(superclass :: interfaces, body))
      })
    }

    def interfaceDecl(mods: Modifiers): List[Tree] = {
      accept(INTERFACE)
      val pos = in.currentPos
      val name = identForType()
      val tparams = typeParams()
      val parents =
        if (in.token == EXTENDS) {
          in.nextToken()
          repsep(typ, COMMA)
        } else {
          List(javaLangObject())
        }
      val (statics, body) = typeBody(INTERFACE, name)
      addCompanionObject(statics, atPos(pos) {
        ClassDef(mods | Flags.TRAIT | Flags.INTERFACE | Flags.ABSTRACT,
                 name, tparams,
                 makeTemplate(parents, body))
      })
    }

    def typeBody(leadingToken: Int, parentName: Name): (List[Tree], List[Tree]) = {
      accept(LBRACE)
      val defs = typeBodyDecls(leadingToken, parentName)
      accept(RBRACE)
      defs
    }

    def typeBodyDecls(parentToken: Int, parentName: Name): (List[Tree], List[Tree]) = {
      val inInterface = definesInterface(parentToken)
      val statics = new ListBuffer[Tree]
      val members = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        var mods = modifiers(inInterface)
        if (in.token == LBRACE) {
          skipAhead() // skip init block, we just assume we have seen only static
          accept(RBRACE)
        } else if (in.token == SEMI) {
          in.nextToken()
        } else {
          if (in.token == ENUM || definesInterface(in.token)) mods |= Flags.STATIC
          val decls = memberDecl(mods, parentToken)
          (if (mods.hasStaticFlag || inInterface && !(decls exists (_.isInstanceOf[DefDef])))
             statics
           else
             members) ++= decls
        }
      }
      def forwarders(sdef: Tree): List[Tree] = sdef match {
        case ClassDef(mods, name, tparams, _) if (parentToken == INTERFACE) =>
          val tparams1: List[TypeDef] = tparams map (_.duplicate)
          var rhs: Tree = Select(Ident(parentName.toTermName), name)
          if (!tparams1.isEmpty) rhs = AppliedTypeTree(rhs, tparams1 map (tp => Ident(tp.name)))
          List(TypeDef(Modifiers(Flags.PROTECTED), name, tparams1, rhs))
        case _ =>
          List()
      }
      val sdefs = statics.toList
      val idefs = members.toList ::: (sdefs flatMap forwarders)
      (sdefs, idefs)
    }
    def annotationParents = List(
      gen.scalaAnnotationDot(tpnme.Annotation),
      Select(javaLangDot(nme.annotation), tpnme.Annotation),
      gen.scalaAnnotationDot(tpnme.ClassfileAnnotation)
    )
    def annotationDecl(mods: Modifiers): List[Tree] = {
      accept(AT)
      accept(INTERFACE)
      val pos = in.currentPos
      val name = identForType()
      val (statics, body) = typeBody(AT, name)
      val templ = makeTemplate(annotationParents, body)
      addCompanionObject(statics, atPos(pos) {
        ClassDef(mods | Flags.JAVA_ANNOTATION, name, List(), templ)
      })
    }

    def enumDecl(mods: Modifiers): List[Tree] = {
      accept(ENUM)
      val pos = in.currentPos
      val name = identForType()
      def enumType = Ident(name)
      val interfaces = interfacesOpt()
      accept(LBRACE)
      val buf = new ListBuffer[Tree]
      var enumIsFinal = true
      def parseEnumConsts() {
        if (in.token != RBRACE && in.token != SEMI && in.token != EOF) {
          val (const, hasClassBody) = enumConst(enumType)
          buf += const
          // if any of the enum constants has a class body, the enum class is not final (JLS 8.9.)
          enumIsFinal &&= !hasClassBody
          if (in.token == COMMA) {
            in.nextToken()
            parseEnumConsts()
          }
        }
      }
      parseEnumConsts()
      val consts = buf.toList
      val (statics, body) =
        if (in.token == SEMI) {
          in.nextToken()
          typeBodyDecls(ENUM, name)
        } else {
          (List(), List())
        }
      val predefs = List(
        DefDef(
          Modifiers(Flags.JAVA | Flags.STATIC), nme.values, List(),
          ListOfNil,
          arrayOf(enumType),
          blankExpr),
        DefDef(
          Modifiers(Flags.JAVA | Flags.STATIC), nme.valueOf, List(),
          List(List(makeParam("x", TypeTree(StringTpe)))),
          enumType,
          blankExpr))
      accept(RBRACE)
      val superclazz =
        AppliedTypeTree(javaLangDot(tpnme.Enum), List(enumType))
      val finalFlag = if (enumIsFinal) Flags.FINAL else 0l
      val abstractFlag = {
        // javac adds `ACC_ABSTRACT` to enum classes with deferred members
        val hasAbstractMember = body exists {
          case d: DefDef => d.mods.isDeferred
          case _         => false
        }
        if (hasAbstractMember) Flags.ABSTRACT else 0l
      }
      addCompanionObject(consts ::: statics ::: predefs, atPos(pos) {
        ClassDef(mods | Flags.JAVA_ENUM | finalFlag | abstractFlag, name, List(),
                 makeTemplate(superclazz :: interfaces, body))
      })
    }

    def enumConst(enumType: Tree): (ValDef, Boolean) = {
      annotations()
      var hasClassBody = false
      val res = atPos(in.currentPos) {
        val name = ident()
        if (in.token == LPAREN) {
          // skip arguments
          skipAhead()
          accept(RPAREN)
        }
        if (in.token == LBRACE) {
          hasClassBody = true
          // skip classbody
          skipAhead()
          accept(RBRACE)
        }
        ValDef(Modifiers(Flags.JAVA_ENUM | Flags.STABLE | Flags.JAVA | Flags.STATIC), name.toTermName, enumType, blankExpr)
      }
      (res, hasClassBody)
    }

    def typeDecl(mods: Modifiers): List[Tree] = in.token match {
      case ENUM      => enumDecl(mods)
      case INTERFACE => interfaceDecl(mods)
      case AT        => annotationDecl(mods)
      case CLASS     => classDecl(mods)
      case _         => in.nextToken(); syntaxError("illegal start of type declaration", skipIt = true); List(errorTypeTree)
    }

    /** CompilationUnit ::= [package QualId semi] TopStatSeq
     */
    def compilationUnit(): Tree = {
      var pos = in.currentPos
      val pkg: RefTree =
        if (in.token == AT || in.token == PACKAGE) {
          annotations()
          pos = in.currentPos
          accept(PACKAGE)
          val pkg = qualId()
          accept(SEMI)
          pkg
        } else {
          Ident(nme.EMPTY_PACKAGE_NAME)
        }
      thisPackageName = gen.convertToTypeName(pkg) match {
        case Some(t)  => t.name.toTypeName
        case _        => tpnme.EMPTY
      }
      val buf = new ListBuffer[Tree]
      while (in.token == IMPORT)
        buf ++= importDecl()
      while (in.token != EOF && in.token != RBRACE) {
        while (in.token == SEMI) in.nextToken()
        if (in.token != EOF)
          buf ++= typeDecl(modifiers(inInterface = false))
      }
      accept(EOF)
      atPos(pos) {
        makePackaging(pkg, buf.toList)
      }
    }
  }
}
