/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

//todo: allow infix type patterns


package scala.tools.nsc
package javac

import symtab.Flags
import JavaTokens._
import scala.annotation._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.reflect.internal.util.{CodeAction, ListOfNil, Position}
import scala.tools.nsc.Reporting.WarningCategory
import scala.util.chaining._

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
    def deprecationWarning(off: Int, msg: String, since: String, actions: List[CodeAction]) = runReporting.deprecationWarning(off, msg, since, site = "", origin = "", actions)
    implicit def i2p(offset : Int) : Position = Position.offset(unit.source, offset)
    def warning(pos : Int, msg : String) : Unit = runReporting.warning(pos, msg, WarningCategory.JavaSource, site = "")
    def syntaxError(pos: Int, msg: String) : Unit = reporter.error(pos, msg)
  }

  abstract class JavaParser extends ParserCommon {
    val in: JavaScanner
    def unit: CompilationUnit

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

    protected def skip(): Unit = {
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
    def syntaxError(msg: String, skipIt: Boolean): Unit = {
      syntaxError(in.currentPos, msg, skipIt)
    }

    def syntaxError(pos: Int, msg: String, skipIt: Boolean): Unit = {
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

    def javaLangRecord(): Tree = javaLangDot(tpnme.Record)

    def arrayOf(tpt: Tree) =
      AppliedTypeTree(scalaDot(tpnme.Array), List(tpt))

    def blankExpr = EmptyTree

    def makePackaging(pkg: RefTree, stats: List[Tree]): PackageDef =
      atPos(pkg.pos) {  PackageDef(pkg, stats) }

    def makeTemplate(parents: List[Tree], stats: List[Tree]) =
      Template(parents, noSelfType, if (treeInfo.firstConstructor(stats) == EmptyTree)
        makeConstructor(Nil) :: stats else stats)

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

    /** A hook for joining the comment associated with a definition.
      * Overridden by scaladoc.
      */
    def joinComment(trees: => List[Tree]): List[Tree] = trees

    // ------------- general parsing ---------------------------

    /** skip parent or brace enclosed sequence of things */
    def skipAhead(): Unit = {
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

    def skipTo(tokens: Int*): Unit = {
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

    def acceptClosingAngle(): Unit = {
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

    def qualId(orClassLiteral: Boolean = false): Tree = {
      var t: Tree = atPos(in.currentPos) { Ident(ident()) }
      var done = false
      while (!done && in.token == DOT) {
        in.nextToken()
        t = atPos(in.currentPos) {
          if (orClassLiteral && in.token == CLASS) {
            in.nextToken()
            done = true
            val tpeArg = convertToTypeId(t)
            TypeApply(Select(gen.mkAttributedRef(definitions.PredefModule), nme.classOf), tpeArg :: Nil)
          } else {
            Select(t, ident())
          }
        }
      }
      t
    }

    @tailrec
    final def optArrayBrackets(tpt: Tree): Tree =
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

    def typ(): Tree = {
      annotations() // TODO: fix scala/bug#9883 (JSR 308)
      optArrayBrackets {
        if (in.token == FINAL) in.nextToken()
        if (in.token == IDENTIFIER) {
          var t = typeArgs(atPos(in.currentPos)(Ident(ident())))
          // typeSelect generates Select nodes if the lhs is an Ident or Select,
          // SelectFromTypeTree otherwise. See #3567.
          // Select nodes can be later
          // converted in the typechecker to SelectFromTypeTree if the class
          // turns out to be an instance inner class instead of a static inner class.
          def typeSelect(t: Tree, name: Name) = t match {
            case Ident(_) | Select(_, _) => Select(t, name)
            case _ => SelectFromTypeTree(t, name.toTypeName)
          }
          if (in.token == DOT)
            t.updateAttachment(RootSelection)
          while (in.token == DOT) {
            in.nextToken()
            annotations() // TODO: fix scala/bug#9883 (JSR 308)
            t = typeArgs(atPos(in.currentPos)(typeSelect(t, ident())))
          }
          convertToTypeId(t)
        } else {
          basicType()
        }
      }
    }

    def typeArgs(t: Tree): Tree = {
      val wildcards = new ListBuffer[TypeDef]
      def typeArg(): Tree =
        if (in.token == QMARK) {
          val pos = in.currentPos
          in.nextToken()
          val hi = if (in.token == EXTENDS) { in.nextToken() ; typ() } else Ident(definitions.ObjectClass)
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
        val args = repsep(() => typeArg(), COMMA)
        acceptClosingAngle()
        atPos(t1.pos) {
          val t2: Tree = AppliedTypeTree(t1, args)
          if (wildcards.isEmpty) t2
          else ExistentialTypeTree(t2, wildcards.toList)
        }
      } else t
    }

    def annotations(): List[Tree] = {
      val annots = new ListBuffer[Tree]
      while (in.token == AT) {
        in.nextToken()
        val annot = annotation()
        if (annot.nonEmpty) annots += annot
      }
      annots.toList
    }

    /**
     * Annotation ::= NormalAnnotation
     *              | MarkerAnnotation
     *              | SingleElementAnnotation
     *
     *  NormalAnnotation     ::= `@` TypeName `(` [ElementValuePairList] `)`
     *  ElementValuePairList ::= ElementValuePair {`,` ElementValuePair}
     *  ElementValuePair     ::= Identifier = ElementValue
     *  ElementValue         ::= ConditionalExpressionSubset
     *                         | ElementValueArrayInitializer
     *                         | Annotation
     *
     *  // We only support a subset of the Java syntax that can form constant expressions.
     *  //   https://docs.oracle.com/javase/specs/jls/se14/html/jls-15.html#jls-15.29
     *  //
     *  // Luckily, we can just parse matching `(` and `)` to find our way to the end of the argument list.
     *  // and drop the arguments until we implement full support for Java constant expressions
     *  //
     *  ConditionalExpressionSubset := Literal
     *                               | Identifier
     *                               | QualifiedName
     *                               | ClassLiteral
     *
     * ElementValueArrayInitializer ::= `{` [ElementValueList] [`,`] `}`
     * ElementValueList             ::= ElementValue {`,` ElementValue}
     */
    def annotation(): Tree = {
      object LiteralK { def unapply(@unused token: Token) = tryLiteral() }

      def elementValue(): Tree = in.token match {
        case LiteralK(k) => in.nextToken(); atPos(in.currentPos)(Literal(k))
        case IDENTIFIER  => qualId(orClassLiteral = true)
        case LBRACE      => accept(LBRACE); elementArray()
        case AT          => accept(AT); annotation()
        case _           => in.nextToken(); EmptyTree
      }

      def elementArray(): Tree = atPos(in.pos) {
        val ts = new ListBuffer[Tree]
        while (in.token != RBRACE) {
          ts += elementValue()
          if (in.token == COMMA) in.nextToken() // done this way trailing commas are supported
        }
        val ok = !ts.contains(EmptyTree)
        in.token match {
          case RBRACE if ok => accept(RBRACE); Apply(ArrayModule_overloadedApply, ts.toList: _*)
          case _            => skipTo(RBRACE); EmptyTree
        }
      }

      // 1) name = value
      // 2) implicit `value` arg with constant value
      // 3) implicit `value` arg
      def annArg(): Tree = {
        def mkNamedArg(name: Ident, value: Tree) = if (value.isEmpty) EmptyTree else gen.mkNamedArg(name, value)
        in.token match {
          case IDENTIFIER => qualId(orClassLiteral = true) match {
            case name: Ident if in.token == EQUALS => accept(EQUALS); mkNamedArg(name, elementValue())
            case rhs                               =>                 mkNamedArg(Ident(nme.value), rhs)
          }
          case _ => mkNamedArg(Ident(nme.value), elementValue())
        }
      }

      atPos(in.pos) {
        val id = convertToTypeId(qualId())
        in.token match {
          case LPAREN =>
            // TODO: fix copyFrom+skipAhead; CharArrayReaderData missing
            val saved = new JavaTokenData {}.copyFrom(in) // prep to bail if non-literals/identifiers
            accept(LPAREN)
            val args = in.token match {
              case RPAREN => Nil
              case _      => commaSeparated(atPos(in.pos)(annArg()))
            }
            val ok = !args.contains(EmptyTree)
            in.token match {
              case RPAREN if ok => accept(RPAREN); New(id, List(args))
              case _            => in.copyFrom(saved); skipAhead(); accept(RPAREN); EmptyTree
            }
          case _ => New(id, ListOfNil)
        }
      }
    }

    def modifiers(inInterface: Boolean, annots0: List[Tree] = Nil): Modifiers = {
      var flags: Long = Flags.JAVA
      // assumed true unless we see public/private/protected
      var isPackageAccess = true
      var annots: List[Tree] = annots0
      def addAnnot(sym: Symbol) = annots :+= New(sym.tpe)

      while (true) {
        in.token match {
          case AT if in.lookaheadToken != INTERFACE =>
            in.nextToken()
            val annot = annotation()
            if (annot.nonEmpty) annots :+= annot
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
          case STRICTFP =>
            addAnnot(ScalaStrictFPAttr)
            in.nextToken()
          case SYNCHRONIZED =>
            in.nextToken()
          case _ =>
            val unsealed = 0L   // no flag for UNSEALED
            def consume(added: FlagSet): false = { in.nextToken(); flags |= added; false }
            def lookingAhead(s: String): Boolean = {
              import scala.reflect.internal.Chars._
              var i = 0
              val n = s.length
              val lookahead = in.in.lookahead
              while (i < n && lookahead.ch != SU) {
                if (lookahead.ch != s.charAt(i)) return false
                lookahead.next()
                i += 1
              }
              i == n && Character.isWhitespace(lookahead.ch)
            }
            val done = (in.token != IDENTIFIER) || (
              in.name match {
                case nme.javaRestrictedIdentifiers.SEALED => consume(Flags.SEALED)
                case nme.javaRestrictedIdentifiers.UNSEALED => consume(unsealed)
                case nme.javaRestrictedIdentifiers.NON =>
                  !lookingAhead("-sealed") || {
                    in.nextToken()
                    in.nextToken()
                    consume(unsealed)
                  }
                case _ => true
              }
            )
            if (done) {
              val privateWithin: TypeName =
                if (isPackageAccess && !inInterface) thisPackageName
                else tpnme.EMPTY
              return Modifiers(flags, privateWithin) withAnnotations annots
            }
        }
      }
      abort("should not be here")
    }

    def typeParams(): List[TypeDef] =
      if (in.token == LT) {
        in.nextToken()
        val tparams = repsep(() => typeParam(), COMMA)
        acceptClosingAngle()
        tparams
      } else List()

    def typeParam(): TypeDef =
      atPos(in.currentPos) {
        val anns = annotations()
        val name = identForType()
        val hi = if (in.token == EXTENDS) { in.nextToken() ; bound() } else EmptyTree
        TypeDef(Modifiers(Flags.JAVA | Flags.DEFERRED | Flags.PARAM, tpnme.EMPTY, anns), name, Nil, TypeBoundsTree(EmptyTree, hi))
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
      val vparams = if (in.token == RPAREN) List() else repsep(() => formalParam(), COMMA)
      accept(RPAREN)
      vparams
    }

    def formalParam(): ValDef = {
      if (in.token == FINAL) in.nextToken()
      val anns = annotations()
      var t = typ()
      if (in.token == DOTDOTDOT) {
        in.nextToken()
        t = atPos(t.pos) {
          AppliedTypeTree(scalaDot(tpnme.JAVA_REPEATED_PARAM_CLASS_NAME), List(t))
        }
      }
     varDecl(in.currentPos, Modifiers(Flags.JAVA | Flags.PARAM, typeNames.EMPTY, anns), t, ident().toTermName)
    }

    def optThrows(): Unit = {
      if (in.token == THROWS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      }
    }

    def methodBody(): Tree = {
      skipAhead()
      accept(RBRACE) // skip block
      blankExpr
    }

    def definesInterface(token: Int) = token == INTERFACE || token == AT

    /** If the next token is the identifier "record", convert it into a proper
      * token. Technically, "record" is just a restricted identifier. However,
      * once we've figured out that it is in a position where it identifies a
      * "record" class, it is much more convenient to promote it to a token.
      */
    def adaptRecordIdentifier(): Unit = {
      if (in.token == IDENTIFIER && in.name == nme.javaRestrictedIdentifiers.RECORD)
        in.token = RECORD
    }

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
      } else if (in.token == LBRACE && rtptName != nme.EMPTY && parentToken == RECORD) {
        // compact constructor
        methodBody()
        List.empty
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
          val isConcreteInterfaceMethod = !inInterface || (mods hasFlag Flags.JAVA_DEFAULTMETHOD) || (mods hasFlag Flags.STATIC) || (mods hasFlag Flags.PRIVATE)
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
          // for abstract methods (of classes), the `DEFERRED` flag is already set.
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

      /* Tries to detect final static literals syntactically and returns a constant type replacement */
      def optConstantTpe(): Tree = {
        def constantTpe(const: Constant): Tree = TypeTree(ConstantType(const))

        def forConst(const: Constant): Tree = {
          in.nextToken()
          if (in.token != SEMI) tpt1
          else {
            def isStringTyped = tpt1 match {
              case Ident(TypeName("String")) => true
              case _ => false
            }
            if (const.tag == StringTag && isStringTyped) constantTpe(const)
            else if (tpt1.tpe != null && (const.tag == BooleanTag || const.isNumeric)) {
              // for example, literal 'a' is ok for float. 127 is ok for byte, but 128 is not.
              val converted = const.convertTo(tpt1.tpe)
              if (converted == null) tpt1
              else constantTpe(converted)
            } else tpt1
          }
        }

        in.nextToken() // EQUALS
        if (mods.hasFlag(Flags.STATIC) && mods.isFinal) {
          val neg = in.token match {
            case MINUS | BANG => in.nextToken(); true
            case _ => false
          }
          tryLiteral(neg).map(forConst).getOrElse(tpt1)
        } else tpt1
      }

      val tpt2: Tree =
        if (in.token == EQUALS && !mods.isParameter) {
          val res = optConstantTpe()
          skipTo(COMMA, SEMI)
          res
        } else tpt1

      val mods1 = if (mods.isFinal) mods &~ Flags.FINAL else mods | Flags.MUTABLE
      atPos(pos) {
        ValDef(mods1, name, tpt2, blankExpr)
      }
    }

    def memberDecl(mods: Modifiers, parentToken: Int): List[Tree] = {
      in.token match {
        case CLASS | ENUM | RECORD | INTERFACE | AT =>
          typeDecl(mods)
        case _ =>
          termDecl(mods, parentToken)
      }
    }

    def makeCompanionObject(cdef: ClassDef, statics: List[Tree]): Tree =
      atPos(cdef.pos) {
        ModuleDef(cdef.mods & (Flags.AccessFlags | Flags.JAVA), cdef.name.toTermName,
                  makeTemplate(List(), statics))
      }

    def addCompanionObject(statics: List[Tree], cdef: ClassDef): List[Tree] =
      List(makeCompanionObject(cdef, statics), cdef)

    def importDecl(): List[Tree] = {
      accept(IMPORT)
      val pos = in.currentPos
      val buf = new ListBuffer[Name]
      @tailrec
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
      if (names.lengthIs < 2) {
        syntaxError(pos, "illegal import", skipIt = false)
        List()
      } else {
        val qual = names.tail.init.foldLeft(Ident(names.head): Tree)(Select(_, _))
        val lastname = names.last
        val selector = lastname match {
          case nme.WILDCARD => ImportSelector.wildAt(lastnameOffset)
          case _            => ImportSelector(lastname, lastnameOffset, lastname, lastnameOffset)
        }
        List(atPos(pos)(Import(qual, List(selector))))
      }
    }

    def interfacesOpt() =
      if (in.token == IMPLEMENTS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      } else {
        List()
      }

    def permitsOpt() =
      if (in.token == IDENTIFIER && in.name == nme.javaRestrictedIdentifiers.PERMITS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      }
      else Nil

    def classDecl(mods: Modifiers): List[Tree] = {
      if (mods.hasFlag(SEALED)) patmat.javaClassesByUnit(unit.source) = mutable.Set.empty
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
      val permits = permitsOpt()
      val (statics, body) = typeBody(CLASS)
      addCompanionObject(statics, atPos(pos) {
        ClassDef(mods, name, tparams, makeTemplate(superclass :: interfaces, body))
          .tap(cd => if (permits.nonEmpty) cd.updateAttachment(PermittedSubclasses(permits)))
      })
    }

    def recordDecl(mods: Modifiers): List[Tree] = {
      accept(RECORD)
      val pos = in.currentPos
      val name = identForType()
      val tparams = typeParams()
      val header = formalParams()
      val superclass = javaLangRecord()
      val interfaces = interfacesOpt()
      val (statics, body) = typeBody(RECORD)

      // Generate accessors, if not already explicitly specified. Record bodies tend to be trivial.
      val existing = body.iterator.collect { case DefDef(_, name, Nil, ListOfNil, _, _) => name }.toSet
      val accessors = header.iterator
        .collect {
          case ValDef(mods, name, tpt, _) if !existing(name) =>
            DefDef(Modifiers(Flags.JAVA).withAnnotations(mods.annotations), name, tparams = Nil, vparamss = ListOfNil, tpt.duplicate, blankExpr)
        }
        .toList

      // Generate canonical constructor. During parsing this is done unconditionally but the symbol
      // is unlinked in Namer if it is found to clash with a manually specified constructor.
      val canonicalCtor = DefDef(
        mods | Flags.SYNTHETIC,
        nme.CONSTRUCTOR,
        List(),
        List(header.map(_.duplicate)),
        TypeTree(),
        blankExpr
      )

      addCompanionObject(statics, atPos(pos) {
        ClassDef(
          mods | Flags.FINAL,
          name,
          tparams,
          makeTemplate(superclass :: interfaces, canonicalCtor :: accessors ::: body)
        )
      })
    }

    def interfaceDecl(mods: Modifiers): List[Tree] = {
      if (mods.hasFlag(SEALED)) patmat.javaClassesByUnit(unit.source) = mutable.Set.empty
      accept(INTERFACE)
      val pos = in.currentPos
      val name = identForType()
      val tparams = typeParams()
      val parents =
        if (in.token == EXTENDS) {
          in.nextToken()
          repsep(() => typ(), COMMA)
        } else {
          List(javaLangObject())
        }
      val permits = permitsOpt()
      val (statics, body) = typeBody(INTERFACE)
      addCompanionObject(statics, atPos(pos) {
        ClassDef(mods | Flags.TRAIT | Flags.INTERFACE | Flags.ABSTRACT,
                 name, tparams,
                 makeTemplate(parents, body))
          .tap(cd => if (permits.nonEmpty) cd.updateAttachment(PermittedSubclasses(permits)))
      })
    }

    def typeBody(leadingToken: Int): (List[Tree], List[Tree]) = {
      accept(LBRACE)
      val defs = typeBodyDecls(leadingToken)
      accept(RBRACE)
      defs
    }

    def typeBodyDecls(parentToken: Int): (List[Tree], List[Tree]) = {
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
          // See "14.3. Local Class and Interface Declarations"
          adaptRecordIdentifier()
          if (in.token == ENUM || in.token == RECORD || definesInterface(in.token))
            mods |= Flags.STATIC
          val decls = joinComment(memberDecl(mods, parentToken))

          @tailrec
          def isDefDef(tree: Tree): Boolean = tree match {
            case _: DefDef => true
            case DocDef(_, defn) => isDefDef(defn)
            case _ => false
          }

          (if (mods.hasStaticFlag || inInterface && !(decls exists isDefDef))
             statics
           else
             members) ++= decls
        }
      }
      (statics.toList, members.toList)
    }
    def annotationParents = Select(javaLangDot(nme.annotation), tpnme.Annotation) :: Nil
    def annotationDecl(mods: Modifiers): List[Tree] = {
      accept(AT)
      accept(INTERFACE)
      val pos = in.currentPos
      val name = identForType()
      val (statics, body) = typeBody(AT)
      val templ = makeTemplate(annotationParents, body)
      addCompanionObject(statics, atPos(pos) {
        import Flags._
        ClassDef(
          mods | JAVA_ANNOTATION | TRAIT | INTERFACE | ABSTRACT,
          name, List(), templ)
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
      @tailrec
      def parseEnumConsts(): Unit = {
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
          typeBodyDecls(ENUM)
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
      val hasAbstractMember = body.exists {
        case m: MemberDef => m.mods.hasFlag(Flags.DEFERRED)
        case _ => false
      }
      val finalFlag = if (enumIsFinal) Flags.FINAL else 0L
      val abstractFlag = if (hasAbstractMember) Flags.ABSTRACT else 0L
      addCompanionObject(consts ::: statics ::: predefs, atPos(pos) {
        ClassDef(mods | Flags.JAVA_ENUM | Flags.SEALED | abstractFlag | finalFlag, name, List(),
                 makeTemplate(superclazz :: interfaces, body))
      })
    }

    def enumConst(enumType: Tree): (ValDef, Boolean) = {
      val anns = annotations()
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
        ValDef(Modifiers(Flags.JAVA_ENUM | Flags.STABLE | Flags.JAVA | Flags.STATIC, typeNames.EMPTY, anns), name.toTermName, enumType, blankExpr)
      }
      (res, hasClassBody)
    }

    def typeDecl(mods: Modifiers): List[Tree] = {
      adaptRecordIdentifier()
      in.token match {
        case ENUM      => joinComment(enumDecl(mods))
        case INTERFACE => joinComment(interfaceDecl(mods))
        case AT        => annotationDecl(mods)
        case CLASS     => joinComment(classDecl(mods))
        case RECORD    => joinComment(recordDecl(mods))
        case _         => in.nextToken(); syntaxError("illegal start of type declaration", skipIt = true); List(errorTypeTree)
      }
    }

    def tryLiteral(negate: Boolean = false): Option[Constant] = {
      val l = in.token match {
        case TRUE      => !negate
        case FALSE     => negate
        case CHARLIT   => in.name.charAt(0)
        case INTLIT    => in.intVal(negate).toInt
        case LONGLIT   => in.intVal(negate)
        case FLOATLIT  => in.floatVal(negate).toFloat
        case DOUBLELIT => in.floatVal(negate)
        case STRINGLIT => in.name.toString
        case _         => null
      }
      if (l == null) None
      else Some(Constant(l))
    }

    /** CompilationUnit ::= [[Annotation] package QualId semi] {Import} {TypeDecl} //TopStatSeq
     */
    def compilationUnit(): Tree = {
      val buf = ListBuffer.empty[Tree]
      var pos = in.currentPos
      val leadingAnnots = if (in.token == AT) annotations() else Nil
      val pkg: RefTree =
        if (in.token == PACKAGE) {
          if (!leadingAnnots.isEmpty) { // TODO: put these somewhere?
            //if (unit.source.file.name != "package-info.java")
            //  syntaxError(pos, "package annotations must be in file package-info.java")
            pos = in.currentPos
          }
          accept(PACKAGE)
          qualId().asInstanceOf[RefTree].tap(_ => accept(SEMI))
        }
        else {
          if (!leadingAnnots.isEmpty)
            buf ++= typeDecl(modifiers(inInterface = false, annots0 = leadingAnnots))
          Ident(nme.EMPTY_PACKAGE_NAME)
        }
      thisPackageName = gen.convertToTypeName(pkg) match {
        case Some(t)  => t.name.toTypeName
        case _        => tpnme.EMPTY
      }
      if (buf.isEmpty)
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
