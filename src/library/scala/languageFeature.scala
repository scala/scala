/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import scala.annotation.meta

object languageFeature {

  @meta.languageFeature("extension of type scala.Dynamic", enableRequired = true)
  sealed trait dynamics
  object dynamics extends dynamics

  @meta.languageFeature("postfix operator #", enableRequired = false)
  sealed trait postfixOps
  object postfixOps extends postfixOps

  @meta.languageFeature("reflective access of structural type member #", enableRequired = false)
  sealed trait reflectiveCalls
  object reflectiveCalls extends reflectiveCalls

  @meta.languageFeature("implicit conversion #", enableRequired = false)
  sealed trait implicitConversions
  object implicitConversions extends implicitConversions

  @meta.languageFeature("higher-kinded type", enableRequired = false)
  sealed trait higherKinds
  object higherKinds extends higherKinds

  @meta.languageFeature("#, which cannot be expressed by wildcards, ", enableRequired = false)
  sealed trait existentials
  object existentials extends existentials

  object experimental {
    @meta.languageFeature("macro definition", enableRequired = true)
    sealed trait macros
    object macros extends macros
  }
}

