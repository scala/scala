package scala.reflect
package std

import java.io.File
import internal._

trait SymbolCompleters { self: Mirror =>

  class PackageCompleter(owner: Symbol) extends LazyType {
    override def isComplete = true
    override def complete(sym: Symbol) {}

    private val knownMembers = new Scope

    override def findMember(name: Name, excludedFlags: Long, requiredFlags: Long, stableOnly: Boolean): Symbol = {
      if (name == nme.ANYNAME) abort("cannot enumerate members of a package")
      else {
        val sym = knownMembers.lookup(name)
        if (sym != NoSymbol) sym
        else {
          def prepare(sym: Symbol, tpe: Type): Symbol = {
            knownMembers.enter(sym)
            sym.setInfo(tpe)
          }
          try {
            val prefix = if (owner.isRoot) "" else owner.fullName + "."
            val jclazz = java.lang.Class.forName(prefix + name)
            val classRoot = owner.newClass(NoPosition, name.toTypeName)
            val moduleRoot = owner.newModule(NoPosition, name.toTermName)
            val loader = new ClassCompleter(classRoot, moduleRoot, jclazz)
            prepare(classRoot, loader)
            prepare(moduleRoot, loader)
            if (name.isTypeName) classRoot else moduleRoot
          } catch {
            case ex: ClassNotFoundException =>
              if (name.isTermName) {
                // looking for module, construct package symbol instead
                val pkg = owner.newPackage(NoPosition, name)
                prepare(pkg, new PackageCompleter(pkg))
              } else
                NoSymbol
          }
        }
      }
    }
  }

  class ClassCompleter(classRoot: Symbol, moduleRoot: Symbol, jclazz: java.lang.Class[_]) extends LazyType {

    import definitions._

    def scalaSigAttr(jclazz: java.lang.Class[_]): Option[Array[Byte]] =
      jclazz.getAnnotation(classOf[ScalaSignature]) match {
      case sig: ScalaSignature => Some(sig.bytes.getBytes())
      case _ => jclazz.getAnnotation(classOf[ScalaLongSignature]) match {
        case sig: ScalaLongSignature => Some(sig.bytes flatMap (_.getBytes))
        case _ => None
      }
    }

    def readJava(classRoot: Symbol, moduleRoot: Symbol) {
    }

    var _isComplete = false
    override def isComplete = _isComplete

    override def complete(sym: Symbol) {
      _isComplete = true
      scalaSigAttr(jclazz) match {
        case Some(bytes) =>
          val filename = classRoot.fullName.replace('.', File.separatorChar)
          unpickler.unpickle(bytes, 0, classRoot, moduleRoot, filename)
        case None =>
          readJava(classRoot, moduleRoot)
      }
    }
  }

  val rootLoader = new PackageCompleter(definitions.RootClass)
}