/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc
package incremental

import scala.tools.nsc.Global

/**
 * Utility methods for creating (source|binary) class names for a Symbol.
 */
class ClassNameUtils[G <: Global](val global: G) {
 import global._

  /**
   * Creates a flat (binary) name for a class symbol `s`.
   */
  def flatname(s: Symbol, separator: Char) =
    exitingFlatten { s fullName separator }

  /**
   * Create a (source) name for a class symbol `s`.
   */
  def className(s: Symbol): Name = pickledName(s)

  /**
   * Create a String (source) name for a class symbol `s`.
   */
  def classNameAsString(s: Symbol): String = pickledNameAsString(s)

  /**
   * Given a class symbol `cls`, construct a name representing this constructor.
   * For a class:
   *
   *     a.b.Foo
   *
   * this is:
   *
   *     a;b;Foo;init;
   *
   * The prefix is important to avoid name hashing all constructors together
   * (see #97), the weird format is necessary to avoid scalac or zinc trying to
   * interpret this name (in particular we should not use '.' and we should not
   * use '<init>'), we use ';' because it is one of the few characters that
   * cannot appear in a valid JVM name.
   */
  protected def constructorName(cls: Symbol): Name =
    newTermName(constructorNameAsString(cls))

  def constructorNameAsString(cls: Symbol): String =
    cls.fullName(';') ++ ";init;"

  /**
   * Mangle a JVM symbol name in a format better suited for internal uses by sbt.
   */
  def mangledName(s: Symbol): Name =
    if (s.name == nme.CONSTRUCTOR)
      constructorName(s.enclClass)
    else
      s.name

  /**
   * Create a (source) name for the class symbol `s` with a prefix determined by the class symbol `in`.
   *
   * If `s` represents a package object `pkg3`, then the returned name will be `pkg1.pkg2.pkg3.package`.
   * If `s` represents a class `Foo` nested in package object `pkg3` then the returned name is `pkg1.pkg2.pk3.Foo`.
   *
   * Note that some objects with special access rights are encoded in names
   * (like qualified privates `private[qualifier]`). In order to get the right
   * original names, we need to use `unexpandedName`.
   */
  def classNameAsSeenIn(in: Symbol, s: Symbol): String =
    exitingPickler {
      if (in.isRoot || in.isRootPackage || in == NoSymbol || in.isEffectiveRoot)
        s.simpleName.toString
      else if (in.isPackageObjectOrClass)
        in.owner.fullName + "." + s.unexpandedName
      else
        in.fullName + "." + s.unexpandedName
    }

  private def pickledName(s: Symbol): Name =
    exitingPickler { s.fullNameAsName('.') }

  private def pickledNameAsString(s: Symbol): String =
    exitingPickler { s.fullName }

  def isTopLevelModule(sym: Symbol): Boolean =
    exitingPickler {
      // uncomment `isImplClass` when backporting this to 2.11
      sym.isModuleClass && !sym.isNestedClass // && !sym.isImplClass
    }

  def flatclassName(s: Symbol, sep: Char, dollarRequired: Boolean): String =
    flatname(s, sep) + (if (dollarRequired) "$" else "")
}
