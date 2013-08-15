package scala.reflect.macros
package runtime

import scala.reflect.runtime.{universe => ru}

trait ScalaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait ScalaReflectionResolvers {
    self: MacroRuntimeResolver =>

    def resolveScalaReflectionRuntime(classLoader: ClassLoader): MacroRuntime = {
      val macroMirror: ru.JavaMirror = ru.runtimeMirror(classLoader)
      val implContainerSym = macroMirror.classSymbol(Class.forName(className, true, classLoader))
      val implMethSym = implContainerSym.typeSignature.member(ru.TermName(methName)).asMethod
      macroLogVerbose(s"successfully loaded macro impl as ($implContainerSym, $implMethSym)")
      args => {
        val implContainer =
          if (isBundle) {
            val implCtorSym = implContainerSym.typeSignature.member(ru.nme.CONSTRUCTOR).asMethod
            macroMirror.reflectClass(implContainerSym).reflectConstructor(implCtorSym)(args.c)
          } else {
            macroMirror.reflectModule(implContainerSym.module.asModule).instance
          }
        val implMeth = macroMirror.reflect(implContainer).reflectMethod(implMethSym)
        val implArgs = if (isBundle) args.others else args.c +: args.others
        implMeth(implArgs: _*)
      }
    }
  }
}
