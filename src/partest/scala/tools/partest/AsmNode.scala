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

package scala.tools.partest

import scala.jdk.CollectionConverters._
import scala.tools.asm
import asm._
import asm.tree._
import java.lang.reflect.Modifier

sealed trait AsmNode[+T] {
  def node: T
  def access: Int
  def desc: String
  def name: String
  def signature: String
  def attrs: List[Attribute]
  def visibleAnnotations: List[AnnotationNode]
  def invisibleAnnotations: List[AnnotationNode]
  def characteristics = f"$name%15s $desc%-30s$accessString$sigString"
  def erasedCharacteristics = f"$name%15s $desc%-30s$accessString"

  private def accessString     = if (access == 0) "" else " " + Modifier.toString(access)
  private def sigString        = if (signature == null) "" else " " + signature
  override def toString        = characteristics
}

object AsmNode {
  type AsmMethod = AsmNode[MethodNode]
  type AsmField = AsmNode[FieldNode]
  type AsmMember = AsmNode[_]

  implicit class ClassNodeOps(val node: ClassNode) {
    def fieldsAndMethods: List[AsmMember] = {
      val xs: List[AsmMember] = (
           node.methods.asScala.toList.map(x => (x: AsmMethod))
        ++ node.fields.asScala.toList.map(x => (x: AsmField))
      )
      xs sortBy (_.characteristics)
    }
  }
  implicit class AsmMethodNode(val node: MethodNode) extends AsmNode[MethodNode] {
    def access: Int                                = node.access
    def desc: String                               = node.desc
    def name: String                               = node.name
    def signature: String                          = node.signature
    def attrs: List[Attribute]                     = node.attrs.asScala.toList
    def visibleAnnotations: List[AnnotationNode]   = node.visibleAnnotations.asScala.toList
    def invisibleAnnotations: List[AnnotationNode] = node.invisibleAnnotations.asScala.toList
  }
  implicit class AsmFieldNode(val node: FieldNode) extends AsmNode[FieldNode] {
    def access: Int                                = node.access
    def desc: String                               = node.desc
    def name: String                               = node.name
    def signature: String                          = node.signature
    def attrs: List[Attribute]                     = node.attrs.asScala.toList
    def visibleAnnotations: List[AnnotationNode]   = node.visibleAnnotations.asScala.toList
    def invisibleAnnotations: List[AnnotationNode] = node.invisibleAnnotations.asScala.toList
  }

  def apply(node: MethodNode): AsmMethodNode = new AsmMethodNode(node)
  def apply(node: FieldNode): AsmFieldNode   = new AsmFieldNode(node)
}
