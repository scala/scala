/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm
import asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassFileLookup
import OptimizerReporting._

class CodeRepository(val classPath: ClassFileLookup[AbstractFile]) {
  import BTypes.InternalName

  /**
   * Cache for parsed ClassNodes.
   */
  val classes: collection.concurrent.Map[InternalName, ClassNode] = collection.concurrent.TrieMap.empty[InternalName, ClassNode]

  /**
   * The class node for an internal name. If the class node is not yet available, it is parsed from
   * the classfile on the compile classpath.
   */
  def classNode(internalName: InternalName): ClassNode = {
    classes.getOrElseUpdate(internalName, parseClass(internalName))
  }

  /**
   * The field node for a field matching `name` and `descriptor`, accessed in class `classInternalName`.
   * The declaration of the field may be in one of the superclasses.
   *
   * @return The [[FieldNode]] of the requested field and the [[InternalName]] of its declaring class.
   */
  def fieldNode(classInternalName: InternalName, name: String, descriptor: String): Option[(FieldNode, InternalName)] = {
    val c = classNode(classInternalName)
    c.fields.asScala.find(f => f.name == name && f.desc == descriptor).map((_, classInternalName)) orElse {
      Option(c.superName).flatMap(n => fieldNode(n, name, descriptor))
    }
  }

  /**
   * The method node for a method matching `name` and `descriptor`, accessed in class `classInternalName`.
   * The declaration of the method may be in one of the parents.
   *
   * @return The [[MethodNode]] of the requested method and the [[InternalName]] of its declaring class.
   */
  def methodNode(classInternalName: InternalName, name: String, descriptor: String): Option[(MethodNode, InternalName)] = {
    val c = classNode(classInternalName)
    c.methods.asScala.find(m => m.name == name && m.desc == descriptor).map((_, classInternalName)) orElse {
      val parents = Option(c.superName) ++ c.interfaces.asScala
      // `view` to stop at the first result
      parents.view.flatMap(methodNode(_, name, descriptor)).headOption
    }
  }

  private def parseClass(internalName: InternalName): ClassNode = {
    val fullName = internalName.replace('/', '.')
    classPath.findClassFile(fullName) map { classFile =>
      val classNode = new asm.tree.ClassNode()
      val classReader = new asm.ClassReader(classFile.toByteArray)
      // We don't need frames when inlining, but we want to keep the local variable table, so we
      // don't use SKIP_DEBUG.
      classReader.accept(classNode, asm.ClassReader.SKIP_FRAMES)
      // SKIP_FRAMES leaves line number nodes. Remove them because they are not correct after
      // inlining.
      // TODO: we need to remove them also for classes that are not parsed from classfiles, why not simplify and do it once when inlining?
      // OR: instead of skipping line numbers for inlined code, use write a SourceDebugExtension
      // attribute that contains JSR-45 data that encodes debugging info.
      //   http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.11
      //   https://jcp.org/aboutJava/communityprocess/final/jsr045/index.html
      removeLineNumberNodes(classNode)
      classes(internalName) = classNode
      classNode
    } getOrElse {
      inlineFailure(s"Class file for class $fullName not found.")
    }
  }

  private def removeLineNumberNodes(classNode: ClassNode): Unit = {
    for (method <- classNode.methods.asScala) {
      val iter = method.instructions.iterator()
      while (iter.hasNext) iter.next() match {
        case _: LineNumberNode => iter.remove()
        case _ =>
      }
    }
  }
}
