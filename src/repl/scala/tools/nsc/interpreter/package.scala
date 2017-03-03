/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import scala.language.implicitConversions
import scala.reflect.{ classTag, ClassTag }
import scala.reflect.runtime.{ universe => ru }
import scala.reflect.{ClassTag, classTag}
import scala.reflect.api.{Mirror, TypeCreator, Universe => ApiUniverse}
import scala.util.control.Exception.catching
import scala.util.Try

/** The main REPL related classes and values are as follows.
 *  In addition to standard compiler classes Global and Settings, there are:
 *
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  ILoop (formerly InterpreterLoop): The umbrella class for a session.
 *  IMain (formerly Interpreter): Handles the evolving state of the session
 *    and handles submitting code to the compiler and handling the output.
 *  InteractiveReader: how ILoop obtains input.
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  Power: a repository for more advanced/experimental features.
 *
 *  ILoop contains { in: InteractiveReader, intp: IMain, settings: Settings, power: Power }
 *  InteractiveReader contains { history: History, completion: Completion }
 *  IMain contains { global: Global }
 */
package object interpreter extends ReplConfig with ReplStrings {
  type JFile          = java.io.File
  type JClass         = java.lang.Class[_]
  type JList[T]       = java.util.List[T]
  type JCollection[T] = java.util.Collection[T]
  type JPrintWriter   = java.io.PrintWriter
  type InputStream    = java.io.InputStream
  type OutputStream   = java.io.OutputStream

  val IR = Results

  implicit def postfixOps = scala.language.postfixOps // make all postfix ops in this package compile without warning

  private[interpreter] implicit def javaCharSeqCollectionToScala(xs: JCollection[_ <: CharSequence]): List[String] = {
    import scala.collection.JavaConverters._
    xs.asScala.toList map ("" + _)
  }

  private[nsc] implicit def enrichClass[T](clazz: Class[T]) = new RichClass[T](clazz)
  private[nsc] implicit def enrichAnyRefWithTap[T](x: T) = new TapMaker(x)
  private[nsc] def debugging[T](msg: String)(x: T) = x.tapDebug(msg)

  private val ourClassloader = getClass.getClassLoader

  def staticTypeTag[T: ClassTag]: ru.TypeTag[T] = ru.TypeTag[T](
    ru.runtimeMirror(ourClassloader),
    new TypeCreator {
      def apply[U <: ApiUniverse with Singleton](m: Mirror[U]): U # Type =
        m.staticClass(classTag[T].runtimeClass.getName).toTypeConstructor.asInstanceOf[U # Type]
  })

  /** This class serves to trick the compiler into treating a var
   *  (intp, in ILoop) as a stable identifier.
   */
  implicit class IMainOps(val intp: IMain) {
    import intp._
    import global.{ reporter => _, _ }
    import definitions._

    protected def echo(msg: String) = {
      Console.out println msg
      Console.out.flush()
    }

    def implicitsCommand(line: String): String = {
      def p(x: Any) = intp.reporter.printMessage("" + x)

      // If an argument is given, only show a source with that
      // in its name somewhere.
      val args     = line split "\\s+"
      val filtered = intp.implicitSymbolsBySource filter {
        case (source, syms) =>
          (args contains "-v") || {
            if (line == "") (source.fullName.toString != "scala.Predef")
            else (args exists (source.name.toString contains _))
          }
      }

      filtered foreach {
        case (source, syms) =>
          p("/* " + syms.size + " implicit members imported from " + source.fullName + " */")

          // This groups the members by where the symbol is defined
          val byOwner = syms groupBy (_.owner)
          val sortedOwners = byOwner.toList sortBy { case (owner, _) => exitingTyper(source.info.baseClasses indexOf owner) }

          sortedOwners foreach {
            case (owner, members) =>
              // Within each owner, we cluster results based on the final result type
              // if there are more than a couple, and sort each cluster based on name.
              // This is really just trying to make the 100 or so implicits imported
              // by default into something readable.
              val memberGroups: List[List[Symbol]] = {
                val groups = members groupBy (_.tpe.finalResultType) toList
                val (big, small) = groups partition (_._2.size > 3)
                val xss = (
                  (big sortBy (_._1.toString) map (_._2)) :+
                  (small flatMap (_._2))
                )

                xss map (xs => xs sortBy (_.name.toString))
              }

              val ownerMessage = if (owner == source) " defined in " else " inherited from "
              p("  /* " + members.size + ownerMessage + owner.fullName + " */")

              memberGroups foreach { group =>
                group foreach (s => p("  " + intp.symbolDefString(s)))
                p("")
              }
          }
          p("")
      }
      
      if (filtered.nonEmpty) 
        "" // side-effects above
      else if (global.settings.nopredef || global.settings.noimports) 
        "No implicits have been imported."
      else
        "No implicits have been imported other than those in Predef." 

    }

    def kindCommandInternal(expr: String, verbose: Boolean): Unit = {
      val catcher = catching(classOf[MissingRequirementError],
                             classOf[ScalaReflectionException])
      def typeFromTypeString: Option[ClassSymbol] = catcher opt {
        exprTyper.typeOfTypeString(expr).typeSymbol.asClass
      }
      def typeFromNameTreatedAsTerm: Option[ClassSymbol] = catcher opt {
        val moduleClass = exprTyper.typeOfExpression(expr).typeSymbol
        moduleClass.linkedClassOfClass.asClass
      }
      def typeFromFullName: Option[ClassSymbol] = catcher opt {
        intp.global.rootMirror.staticClass(expr)
      }
      def typeOfTerm: Option[TypeSymbol] = replInfo(symbolOfLine(expr)).typeSymbol match {
        case sym: TypeSymbol => Some(sym)
        case _ => None
      }
      (typeFromTypeString orElse typeFromNameTreatedAsTerm orElse typeFromFullName orElse typeOfTerm) foreach { sym =>
        val (kind, tpe) = exitingTyper {
          val tpe = sym.tpeHK
          (intp.global.inferKind(NoPrefix)(tpe, sym.owner), tpe)
        }
        echoKind(tpe, kind, verbose)
      }
    }

    def echoKind(tpe: Type, kind: Kind, verbose: Boolean) {
      def typeString(tpe: Type): String = {
        tpe match {
          case TypeRef(_, sym, _) => typeString(sym.info)
          case RefinedType(_, _)  => tpe.toString
          case _                  => tpe.typeSymbol.fullName
        }
      }
      printAfterTyper(typeString(tpe) + "'s kind is " + kind.scalaNotation)
      if (verbose) {
        echo(kind.starNotation)
        echo(kind.description)
      }
    }

    /** TODO -
     *  -n normalize
     *  -l label with case class parameter names
     *  -c complete - leave nothing out
     */
    def typeCommandInternal(expr: String, verbose: Boolean): Unit =
      symbolOfLine(expr) andAlso (echoTypeSignature(_, verbose))

    def printAfterTyper(msg: => String) =
      reporter printUntruncatedMessage exitingTyper(msg)

    private def replInfo(sym: Symbol) =
      if (sym.isAccessor) dropNullaryMethod(sym.info) else sym.info

    def echoTypeStructure(sym: Symbol) =
      printAfterTyper("" + deconstruct.show(replInfo(sym)))

    def echoTypeSignature(sym: Symbol, verbose: Boolean) = {
      if (verbose) echo("// Type signature")
      printAfterTyper("" + replInfo(sym))

      if (verbose) {
        echo("\n// Internal Type structure")
        echoTypeStructure(sym)
      }
    }
  }

  /* An s-interpolator that uses `stringOf(arg)` instead of `String.valueOf(arg)`. */
  private[nsc] implicit class `smart stringifier`(val sc: StringContext) extends AnyVal {
    import StringContext.treatEscapes, scala.runtime.ScalaRunTime.stringOf
    def ss(args: Any*): String = sc.standardInterpolator(treatEscapes, args map stringOf)
  }
  /* Try (body) lastly (more) */
  private[nsc] implicit class `try lastly`[A](val t: Try[A]) extends AnyVal {
    private def effect[X](last: => Unit)(a: X): Try[A] = { last; t }
    def lastly(last: => Unit): Try[A] = t transform (effect(last) _, effect(last) _)
  }
}
