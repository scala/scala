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

package scala.collection.immutable

import org.scalacheck._

class WrappedStringProperties extends Properties("immutable.WrappedString") {
  import Prop._
  import WrappedStringProperties._

  property("indexOf") = forAll { (s: WrappedString, c: Char) =>
    s.indexOf(c) == s.toString.indexOf(c)
  }

  property("indexOf with start") = forAll { (s: WrappedString, start: Int, c: Char) =>
    start < s.size ==> (s.indexOf(c, start) == s.toString.indexOf(c, start))
  }

  property("indexOf") = forAll { (s: WrappedString, c: Char) =>
    s.lastIndexOf(c) == s.toString.lastIndexOf(c)
  }

  property("indexOf with end") = forAll { (s: WrappedString, end: Int, c: Char) =>
    end < s.size ==> (s.lastIndexOf(c, end) == s.toString.lastIndexOf(c, end))
  }

  property("sameElements") = forAll { (s: WrappedString, o: WrappedString) =>
    s.sameElements(o) == (s.toString == o.toString)
  }

  property("sameElements reflexive") = forAll { (s: WrappedString) =>
    s sameElements s
  }

  property("sameElements neg init") = forAll { (s: WrappedString) =>
    !s.isEmpty ==> !s.sameElements(s.init)
  }

  property("sameElements neg tail") = forAll { (s: WrappedString) =>
    !s.isEmpty ==> !s.sameElements(s.tail)
  }

  property("sameElements neg sizes") = forAll { (s: WrappedString, o: WrappedString) =>
    (s.size != o.size) ==> !s.sameElements(o)
  }

}

object WrappedStringProperties {

  implicit val arbitraryWrappedString: Arbitrary[WrappedString] = Arbitrary {
    for {
      isAlpha <- Gen.oneOf(false, true)
      charGen  = if (isAlpha) Gen.alphaChar else Arbitrary.arbChar.arbitrary
      size    <- Gen.chooseNum(0, 4)
      chars   <- Gen.listOfN(size, charGen)
    } yield wrapString(new String(chars.toArray))
  }

  implicit val arbitraryChar: Arbitrary[Char] = Arbitrary(Gen.alphaNumChar)

}
