/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.collection.JavaConverters._
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{MethodInsnNode, MethodNode}
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.BackendReporting.OptimizerWarning

class InlinerHeuristics[BT <: BTypes](val bTypes: BT) {
  import bTypes._
  import callGraph._

  case class InlineRequest(callsite: Callsite, post: List[InlineRequest], reason: String) {
    // invariant: all post inline requests denote callsites in the callee of the main callsite
    for (pr <- post) assert(pr.callsite.callsiteMethod == callsite.callee.get.callee, s"Callsite method mismatch: main $callsite - post ${pr.callsite}")
  }

  def canInlineFromSource(sourceFilePath: Option[String]) = compilerSettings.optInlineGlobal || sourceFilePath.isDefined

  /**
   * Select callsites from the call graph that should be inlined, grouped by the containing method.
   * Cyclic inlining requests are allowed, the inliner will eliminate requests to break cycles.
   */
  def selectCallsitesForInlining: Map[MethodNode, Set[InlineRequest]] = {
    // We should only create inlining requests for callsites being compiled (not for callsites in
    // classes on the classpath). The call graph may contain callsites of classes parsed from the
    // classpath. In order to get only the callsites being compiled, we start at the map of
    // compilingClasses in the byteCodeRepository.
    val compilingMethods = for {
      (classNode, _) <- byteCodeRepository.compilingClasses.valuesIterator
      methodNode     <- classNode.methods.iterator.asScala
    } yield methodNode

    compilingMethods.map(methodNode => {
      var requests = Set.empty[InlineRequest]
      callGraph.callsites(methodNode).valuesIterator foreach {
        case callsite @ Callsite(_, _, _, Right(Callee(callee, calleeDeclClass, safeToInline, sourceFilePath, calleeAnnotatedInline, _, _, callsiteWarning)), _, _, _, pos, _, _) =>
          inlineRequest(callsite, requests) match {
            case Some(Right(req)) => requests += req
            case Some(Left(w))    =>
              if ((calleeAnnotatedInline && bTypes.compilerSettings.optWarningEmitAtInlineFailed) || w.emitWarning(compilerSettings)) {
                val annotWarn = if (calleeAnnotatedInline) " is annotated @inline but" else ""
                val msg = s"${BackendReporting.methodSignature(calleeDeclClass.internalName, callee)}$annotWarn could not be inlined:\n$w"
                backendReporting.inlinerWarning(callsite.callsitePosition, msg)
              }

            case None =>
              if (canInlineFromSource(sourceFilePath) && calleeAnnotatedInline && !callsite.annotatedNoInline && bTypes.compilerSettings.optWarningEmitAtInlineFailed) {
                // if the callsite is annotated @inline, we report an inline warning even if the underlying
                // reason is, for example, mixed compilation (which has a separate -opt-warning flag).
                def initMsg = s"${BackendReporting.methodSignature(calleeDeclClass.internalName, callee)} is annotated @inline but cannot be inlined"
                def warnMsg = callsiteWarning.map(" Possible reason:\n" + _).getOrElse("")
                if (!safeToInline)
                  backendReporting.inlinerWarning(pos, s"$initMsg: the method is not final and may be overridden." + warnMsg)
                else
                  backendReporting.inlinerWarning(pos, s"$initMsg." + warnMsg)
              } else if (callsiteWarning.isDefined && callsiteWarning.get.emitWarning(compilerSettings)) {
                // when annotatedInline is false, and there is some warning, the callsite metadata is possibly incomplete.
                backendReporting.inlinerWarning(pos, s"there was a problem determining if method ${callee.name} can be inlined: \n"+ callsiteWarning.get)
              }
          }

        case Callsite(ins, _, _, Left(warning), _, _, _, pos, _, _) =>
          if (warning.emitWarning(compilerSettings))
            backendReporting.inlinerWarning(pos, s"failed to determine if ${ins.name} should be inlined:\n$warning")
      }
      (methodNode, requests)
    }).filterNot(_._2.isEmpty).toMap
  }

  /**
   * Returns the inline request for a callsite if the callsite should be inlined according to the
   * current heuristics (`-Yopt-inline-heuristics`).
   *
   * The resulting inline request may contain post-inlining requests of callsites that in turn are
   * also selected as individual inlining requests.
   *
   * @return `None` if this callsite should not be inlined according to the active heuristic
   *         `Some(Left)` if the callsite cannot be inlined (for example because that would cause
   *           an IllegalAccessError) but should be according to the heuristic
   *           TODO: what if a downstream inline request would cause an IAE and we don't create an
   *           InlineRequest for the original callsite? new subclass of OptimizerWarning.
   *         `Some(Right)` if the callsite should be and can be inlined
   */
  def inlineRequest(callsite: Callsite, selectedRequestsForCallee: Set[InlineRequest]): Option[Either[OptimizerWarning, InlineRequest]] = {
    val callee = callsite.callee.get
    def requestIfCanInline(callsite: Callsite, reason: String): Either[OptimizerWarning, InlineRequest] = inliner.earlyCanInlineCheck(callsite) match {
      case Some(w) => Left(w)
      case None =>
        val callee = callsite.callee.get
        val postInlineRequest: List[InlineRequest] = callee.calleeDeclarationClass.isInterface match {
          case Right(true) =>
            // Treat the pair of trait interface method and static method as one for the purposes of inlining:
            // if we inline invokeinterface, invoke the invokestatic, too.
            val calls = callee.callee.instructions.iterator().asScala.filter(BytecodeUtils.isCall).take(2).toList
            calls match {
              case List(x: MethodInsnNode) if x.getOpcode == Opcodes.INVOKESTATIC && x.name == (callee.callee.name + "$") =>
                callGraph.addIfMissing(callee.callee, callee.calleeDeclarationClass)
                val maybeNodeToCallsite1 = callGraph.findCallSite(callee.callee, x)
                maybeNodeToCallsite1.toList.flatMap(x => requestIfCanInline(x, reason).right.toOption)
              case _ =>
                Nil

            }
          case _ => Nil
        }

        Right(InlineRequest(callsite, postInlineRequest, reason))

    }

    compilerSettings.YoptInlineHeuristics.value match {
      case "everything" =>
        if (callee.safeToInline) {
          val reason = if (compilerSettings.YoptLogInline.isSetByUser) "the inline strategy is \"everything\"" else null
          Some(requestIfCanInline(callsite, reason))
        }
        else None

      case "at-inline-annotated" =>
        if (callee.safeToInline && callee.annotatedInline) {
          val reason = if (compilerSettings.YoptLogInline.isSetByUser) {
            val what = if (callee.safeToInline) "callee" else "callsite"
            s"the $what is annotated `@inline`"
          } else null
          Some(requestIfCanInline(callsite, reason))
        }
        else None

      case "default" =>
        if (callee.safeToInline && !callee.annotatedNoInline && !callsite.annotatedNoInline) {
          def shouldInlineHO = callee.samParamTypes.nonEmpty && (callee.samParamTypes exists {
            case (index, _) => callsite.argInfos.contains(index)
          })
          if (callee.annotatedInline || callsite.annotatedInline || shouldInlineHO) {
            val reason = if (compilerSettings.YoptLogInline.isSetByUser) {
              if (callee.annotatedInline || callsite.annotatedInline) {
                val what = if (callee.safeToInline) "callee" else "callsite"
                s"the $what is annotated `@inline`"
              } else {
                val paramNames = Option(callee.callee.parameters).map(_.asScala.map(_.name).toVector)
                def param(i: Int) = {
                  def syn = s"<param $i>"
                  paramNames.fold(syn)(v => v.applyOrElse(i, (_: Int) => syn))
                }
                def samInfo(i: Int, sam: String, arg: String) = s"the argument for parameter (${param(i)}: $sam) is a $arg"
                val argInfos = for ((i, sam) <- callee.samParamTypes; info <- callsite.argInfos.get(i)) yield {
                  val argKind = info match {
                    case FunctionLiteral => "function literal"
                    case ForwardedParam(_) => "parameter of the callsite method"
                  }
                  samInfo(i, sam.internalName.split('/').last, argKind)
                }
                s"the callee is a higher-order method, ${argInfos.mkString(", ")}"
              }
            } else null
            Some(requestIfCanInline(callsite, reason))
          }
          else None
        } else None
    }
  }

  /*
  // using http://lihaoyi.github.io/Ammonite/

  load.ivy("com.google.guava" % "guava" % "18.0")
  val javaUtilFunctionClasses = {
    val rt = System.getProperty("sun.boot.class.path").split(":").find(_.endsWith("lib/rt.jar")).get
    val u = new java.io.File(rt).toURL
    val l = new java.net.URLClassLoader(Array(u))
    val cp = com.google.common.reflect.ClassPath.from(l)
    cp.getTopLevelClasses("java.util.function").toArray.map(_.toString).toList
  }

  // found using IntelliJ's "Find Usages" on the @FunctionalInterface annotation
  val otherClasses = List(
    "com.sun.javafx.css.parser.Recognizer",
    "java.awt.KeyEventDispatcher",
    "java.awt.KeyEventPostProcessor",
    "java.io.FileFilter",
    "java.io.FilenameFilter",
    "java.lang.Runnable",
    "java.lang.Thread$UncaughtExceptionHandler",
    "java.nio.file.DirectoryStream$Filter",
    "java.nio.file.PathMatcher",
    "java.time.temporal.TemporalAdjuster",
    "java.time.temporal.TemporalQuery",
    "java.util.Comparator",
    "java.util.concurrent.Callable",
    "java.util.logging.Filter",
    "java.util.prefs.PreferenceChangeListener",
    "javafx.animation.Interpolatable",
    "javafx.beans.InvalidationListener",
    "javafx.beans.value.ChangeListener",
    "javafx.collections.ListChangeListener",
    "javafx.collections.MapChangeListener",
    "javafx.collections.SetChangeListener",
    "javafx.event.EventHandler",
    "javafx.util.Builder",
    "javafx.util.BuilderFactory",
    "javafx.util.Callback"
  )

  val allClasses = javaUtilFunctionClasses ::: otherClasses

  load.ivy("org.ow2.asm" % "asm" % "5.0.4")
  val classesAndSamNameDesc = allClasses.map(c => {
    val cls = Class.forName(c)
    val internalName = org.objectweb.asm.Type.getDescriptor(cls).drop(1).dropRight(1) // drop L and ;
    val sams = cls.getMethods.filter(m => {
      (m.getModifiers & java.lang.reflect.Modifier.ABSTRACT) != 0 &&
      m.getName != "equals" // Comparator has an abstract override of "equals" for adding Javadoc
    })
    assert(sams.size == 1, internalName + sams.map(_.getName))
    val sam = sams.head
    val samDesc = org.objectweb.asm.Type.getMethodDescriptor(sam)
    (internalName, sam.getName, samDesc)
  })
  println(classesAndSamNameDesc map {
    case (cls, nme, desc) => s"""("$cls", "$nme$desc")"""
  } mkString ("", ",\n", "\n"))
  */
  private val javaSams: Map[String, String] = Map(
    ("java/util/function/BiConsumer", "accept(Ljava/lang/Object;Ljava/lang/Object;)V"),
    ("java/util/function/BiFunction", "apply(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"),
    ("java/util/function/BiPredicate", "test(Ljava/lang/Object;Ljava/lang/Object;)Z"),
    ("java/util/function/BinaryOperator", "apply(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"),
    ("java/util/function/BooleanSupplier", "getAsBoolean()Z"),
    ("java/util/function/Consumer", "accept(Ljava/lang/Object;)V"),
    ("java/util/function/DoubleBinaryOperator", "applyAsDouble(DD)D"),
    ("java/util/function/DoubleConsumer", "accept(D)V"),
    ("java/util/function/DoubleFunction", "apply(D)Ljava/lang/Object;"),
    ("java/util/function/DoublePredicate", "test(D)Z"),
    ("java/util/function/DoubleSupplier", "getAsDouble()D"),
    ("java/util/function/DoubleToIntFunction", "applyAsInt(D)I"),
    ("java/util/function/DoubleToLongFunction", "applyAsLong(D)J"),
    ("java/util/function/DoubleUnaryOperator", "applyAsDouble(D)D"),
    ("java/util/function/Function", "apply(Ljava/lang/Object;)Ljava/lang/Object;"),
    ("java/util/function/IntBinaryOperator", "applyAsInt(II)I"),
    ("java/util/function/IntConsumer", "accept(I)V"),
    ("java/util/function/IntFunction", "apply(I)Ljava/lang/Object;"),
    ("java/util/function/IntPredicate", "test(I)Z"),
    ("java/util/function/IntSupplier", "getAsInt()I"),
    ("java/util/function/IntToDoubleFunction", "applyAsDouble(I)D"),
    ("java/util/function/IntToLongFunction", "applyAsLong(I)J"),
    ("java/util/function/IntUnaryOperator", "applyAsInt(I)I"),
    ("java/util/function/LongBinaryOperator", "applyAsLong(JJ)J"),
    ("java/util/function/LongConsumer", "accept(J)V"),
    ("java/util/function/LongFunction", "apply(J)Ljava/lang/Object;"),
    ("java/util/function/LongPredicate", "test(J)Z"),
    ("java/util/function/LongSupplier", "getAsLong()J"),
    ("java/util/function/LongToDoubleFunction", "applyAsDouble(J)D"),
    ("java/util/function/LongToIntFunction", "applyAsInt(J)I"),
    ("java/util/function/LongUnaryOperator", "applyAsLong(J)J"),
    ("java/util/function/ObjDoubleConsumer", "accept(Ljava/lang/Object;D)V"),
    ("java/util/function/ObjIntConsumer", "accept(Ljava/lang/Object;I)V"),
    ("java/util/function/ObjLongConsumer", "accept(Ljava/lang/Object;J)V"),
    ("java/util/function/Predicate", "test(Ljava/lang/Object;)Z"),
    ("java/util/function/Supplier", "get()Ljava/lang/Object;"),
    ("java/util/function/ToDoubleBiFunction", "applyAsDouble(Ljava/lang/Object;Ljava/lang/Object;)D"),
    ("java/util/function/ToDoubleFunction", "applyAsDouble(Ljava/lang/Object;)D"),
    ("java/util/function/ToIntBiFunction", "applyAsInt(Ljava/lang/Object;Ljava/lang/Object;)I"),
    ("java/util/function/ToIntFunction", "applyAsInt(Ljava/lang/Object;)I"),
    ("java/util/function/ToLongBiFunction", "applyAsLong(Ljava/lang/Object;Ljava/lang/Object;)J"),
    ("java/util/function/ToLongFunction", "applyAsLong(Ljava/lang/Object;)J"),
    ("java/util/function/UnaryOperator", "apply(Ljava/lang/Object;)Ljava/lang/Object;"),
    ("com/sun/javafx/css/parser/Recognizer", "recognize(I)Z"),
    ("java/awt/KeyEventDispatcher", "dispatchKeyEvent(Ljava/awt/event/KeyEvent;)Z"),
    ("java/awt/KeyEventPostProcessor", "postProcessKeyEvent(Ljava/awt/event/KeyEvent;)Z"),
    ("java/io/FileFilter", "accept(Ljava/io/File;)Z"),
    ("java/io/FilenameFilter", "accept(Ljava/io/File;Ljava/lang/String;)Z"),
    ("java/lang/Runnable", "run()V"),
    ("java/lang/Thread$UncaughtExceptionHandler", "uncaughtException(Ljava/lang/Thread;Ljava/lang/Throwable;)V"),
    ("java/nio/file/DirectoryStream$Filter", "accept(Ljava/lang/Object;)Z"),
    ("java/nio/file/PathMatcher", "matches(Ljava/nio/file/Path;)Z"),
    ("java/time/temporal/TemporalAdjuster", "adjustInto(Ljava/time/temporal/Temporal;)Ljava/time/temporal/Temporal;"),
    ("java/time/temporal/TemporalQuery", "queryFrom(Ljava/time/temporal/TemporalAccessor;)Ljava/lang/Object;"),
    ("java/util/Comparator", "compare(Ljava/lang/Object;Ljava/lang/Object;)I"),
    ("java/util/concurrent/Callable", "call()Ljava/lang/Object;"),
    ("java/util/logging/Filter", "isLoggable(Ljava/util/logging/LogRecord;)Z"),
    ("java/util/prefs/PreferenceChangeListener", "preferenceChange(Ljava/util/prefs/PreferenceChangeEvent;)V"),
    ("javafx/animation/Interpolatable", "interpolate(Ljava/lang/Object;D)Ljava/lang/Object;"),
    ("javafx/beans/InvalidationListener", "invalidated(Ljavafx/beans/Observable;)V"),
    ("javafx/beans/value/ChangeListener", "changed(Ljavafx/beans/value/ObservableValue;Ljava/lang/Object;Ljava/lang/Object;)V"),
    ("javafx/collections/ListChangeListener", "onChanged(Ljavafx/collections/ListChangeListener$Change;)V"),
    ("javafx/collections/MapChangeListener", "onChanged(Ljavafx/collections/MapChangeListener$Change;)V"),
    ("javafx/collections/SetChangeListener", "onChanged(Ljavafx/collections/SetChangeListener$Change;)V"),
    ("javafx/event/EventHandler", "handle(Ljavafx/event/Event;)V"),
    ("javafx/util/Builder", "build()Ljava/lang/Object;"),
    ("javafx/util/BuilderFactory", "getBuilder(Ljava/lang/Class;)Ljavafx/util/Builder;"),
    ("javafx/util/Callback", "call(Ljava/lang/Object;)Ljava/lang/Object;")
  )
  def javaSam(internalName: InternalName): Option[String] = javaSams.get(internalName)
}
