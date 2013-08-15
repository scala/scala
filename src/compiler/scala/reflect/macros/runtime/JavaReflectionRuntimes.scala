package scala.reflect.macros
package runtime

import scala.reflect.runtime.ReflectionUtils
import scala.reflect.macros.{Context => ApiContext}

trait JavaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait JavaReflectionResolvers {
    self: MacroRuntimeResolver =>

    def resolveJavaReflectionRuntime(classLoader: ClassLoader): MacroRuntime = {
      val implClass = Class.forName(className, true, classLoader)
      val implMeths = implClass.getDeclaredMethods.find(_.getName == methName)
      // relies on the fact that macro impls cannot be overloaded
      // so every methName can resolve to at maximum one method
      val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
      macroLogVerbose(s"successfully loaded macro impl as ($implClass, $implMeth)")
      args => {
        val implObj =
          if (isBundle) implClass.getConstructor(classOf[ApiContext]).newInstance(args.c)
          else ReflectionUtils.staticSingletonInstance(implClass)
        val implArgs = if (isBundle) args.others else args.c +: args.others
        implMeth.invoke(implObj, implArgs.asInstanceOf[Seq[AnyRef]]: _*)
      }
    }
  }
}