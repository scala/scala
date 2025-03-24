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

package scala.reflect.macros
package runtime

import scala.reflect.runtime.ReflectionUtils
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import java.lang.reflect.{Constructor => jConstructor}

trait JavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait JavaReflectionResolvers {
    self: MacroRuntimeResolver =>

    def resolveJavaReflectionRuntime(classLoader: ClassLoader): MacroRuntime = {
      val implClass = Class.forName(className, true, classLoader)
      val implMeths = implClass.getMethods.find(_.getName == methName)
      // relies on the fact that macro impls cannot be overloaded
      // so every methName can resolve to at maximum one method
      val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
      macroLogVerbose(s"successfully loaded macro impl as ($implClass, $implMeth)")
      if (isBundle) {
        def isMacroContext(clazz: Class[_]) = clazz == classOf[BlackboxContext] || clazz == classOf[WhiteboxContext]

        def isBundleCtor(ctor: jConstructor[_]) = ctor.getParameterTypes match {
          case Array(param) if isMacroContext(param) => true
          case _ => false
        }

        val Array(bundleCtor) = implClass.getConstructors.filter(isBundleCtor): @unchecked
        args => {
          val implObj = bundleCtor.newInstance(args.c)
          implMeth.invoke(implObj, args.others.asInstanceOf[Seq[AnyRef]]: _*)
        }
      } else {
        val implObj = ReflectionUtils.staticSingletonInstance(implClass)
        args => {
          val implArgs = args.c +: args.others
          implMeth.invoke(implObj, implArgs.asInstanceOf[Seq[AnyRef]]: _*)
        }
      }
    }
  }
}
