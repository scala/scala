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

package scala

import scala.annotation.meta

object languageFeature {

  @meta.languageFeature("extension of type scala.Dynamic", enableRequired = true)
  sealed trait dynamics
  object dynamics extends dynamics

  @meta.languageFeature("postfix operator #", enableRequired = true)
  sealed trait postfixOps
  object postfixOps extends postfixOps

  @meta.languageFeature("reflective access of structural type member #", enableRequired = false)
  sealed trait reflectiveCalls
  object reflectiveCalls extends reflectiveCalls

  @meta.languageFeature("implicit conversion #", enableRequired = false)
  sealed trait implicitConversions
  object implicitConversions extends implicitConversions

  @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
  @meta.languageFeature("higher-kinded type", enableRequired = false)
  sealed trait higherKinds
  @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
  object higherKinds extends higherKinds

  @meta.languageFeature("#, which cannot be expressed by wildcards,", enableRequired = false)
  sealed trait existentials
  object existentials extends existentials

  object experimental {
    @meta.languageFeature("macro definition", enableRequired = true)
    sealed trait macros
    object macros extends macros
  }
}

