/*
// To run these tests:
// 1) wget http://scalacheck.googlecode.com/files/ScalaCheck-1.1.1.jar
// 2) scalac -classpath ScalaCheck-1.1.1.jar Either.scala
// 3) scala -classpath .:ScalaCheck-1.1.1.jar scala.CheckEither
// 4) observe > 30000 passing tests

import org.scalacheck.{Arb, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf
import org.scalacheck.{Prop, StdRand}
import org.scalacheck.Prop._
import org.scalacheck.ConsoleReporter.{testReport, propReport}
import org.scalacheck.Test.{Params, check}
import org.scalacheck.ConsoleReporter.testStatsEx
import Function.tupled

object CheckEither {
  implicit def arbitraryEither[X, Y](x: Arb[Either[X, Y]])(implicit xa: Arb[X] => Arbitrary[X], ya: Arb[Y] => Arbitrary[Y]): Arbitrary[Either[X, Y]] =
    new Arbitrary[Either[X, Y]] {
      override def getArbitrary =
        oneOf(arbitrary[X] map(Left(_)), arbitrary[Y] map(Right(_)))
    }

  implicit def arbitraryThrowable(x: Arb[Throwable]): Arbitrary[Throwable] =
    new Arbitrary[Throwable] {
      override def getArbitrary =
        arbitrary[String] map (new Throwable(_))
    }

  val prop_leftE = property((n: Int, s: String) => Left(n).leftE(s) == n)

  val prop_rightE = property((n: Int, s: String) => Right(n).rightE(s) == n)

  val prop_left = property((n: Int) => Left(n).left == n)

  val prop_right = property((n: Int) => Right(n).right == n)

  // unary_~
  val prop_swap = property((e: Either[Int, Int]) => e match {
    case Left(a) => (~e).right == a
    case Right(b) => (~e).left == b
  })

  val prop_isLeft = property((e: Either[Int, Int]) => e match {
    case Left(_) => e.isLeft
    case Right(_) => !e.isLeft
  })

  val prop_isRight = property((e: Either[Int, Int]) => e match {
    case Left(_) => !e.isRight
    case Right(_) => e.isRight
  })

  val prop_either1 = property((n: Int) => Left(n).either(x => x, b => error("fail")) == n)

  val prop_either2 = property((n: Int) => Right(n).either(a => error("fail"), x => x) == n)

  val prop_ifLeft = property((e: Either[Int, Int], n: Int) => {
    var x = 0
    e.ifLeft(x = _)
    e.isRight || x == e.left
  })

  val prop_ifRight = property((e: Either[Int, Int], n: Int) => {
    var x = 0
    e.ifRight(x = _)
    e.isLeft || x == e.right
  })

  val prop_foreach = property((e: Either[Int, Int], n: Int) => {
    var x = 0
    e.foreach(x = _)
    e.isLeft || x == e.right
  })

  val prop_if = property((e: Either[Int, Int], x: Int, y: Int) => e ? (x, y) == (e match {
    case Left(_) => x
    case Right(_) => y
  }))

  val prop_left_ = property((e: Either[Int, Int]) => e.left((_: Int) + 1) == (e match {
    case Left(a) => a
    case Right(b) => b + 1
  }))

  val prop_right_ = property((e: Either[Int, Int]) => e.right((_: Int) + 1) == (e match {
    case Left(a) => a + 1
    case Right(b) => b
  }))

  val prop_leftOr = property((e: Either[Int, Int], or: Int) => e.leftOr(or) == (e match {
    case Left(a) => a
    case Right(_) => or
  }))

  val prop_rightOr = property((e: Either[Int, Int], or: Int) => e.rightOr(or) == (e match {
    case Left(_) => or
    case Right(b) => b
  }))

  val prop_flatMap = property((e: Either[Int, Int]) => e.flatMap(b => Right(b + 1)) == (e match {
    case Left(a) => Left(a)
    case Right(b) => Right(b + 1)
  }))

  // >>=
  val prop_bind = property((e: Either[Int, Int]) => e.>>= (b => Right(b + 1)) == (e match {
    case Left(a) => Left(a)
    case Right(b) => Right(b + 1)
  }))

  // >>
  val prop_anonymousBind = property((e: Either[Int, Int], n: Int) => e >> (Right(n)) == (e match {
    case Left(a) => Left(a)
    case Right(_) => Right(n)
  }))

  val prop_flatMapLeft = property((e: Either[Int, Int]) => e.flatMapLeft(a => Left(a + 1)) == (e match {
    case Left(a) => Left(a + 1)
    case Right(b) => Right(b)
  }))

  // <<=
  val prop_bindLeft = property((e: Either[Int, Int]) => e.<<= (a => Left(a + 1)) == (e match {
    case Left(a) => Left(a + 1)
    case Right(b) => Right(b)
  }))

  // <<
  val prop_anonymousBindLeft = property((e: Either[Int, Int], n: Int) => e << (Left(n)) == (e match {
    case Left(_) => Left(n)
    case Right(b) => Right(b)
  }))

  // <|>
  val prop_mapLeftRight = property((e: Either[Int, Int]) => (e <|> (_ + 1, _ - 1)) == (e match {
    case Left(a) => Left(a + 1)
    case Right(b) => Right(b - 1)
  }))

  val prop_map = property((e: Either[Int, Int]) => (e map (_ + 1)) == (e match {
    case Left(a) => Left(a)
    case Right(b) => Right(b + 1)
  }))

  // |>
  val prop_fmap = property((e: Either[Int, Int]) => (e |> (_ + 1)) == (e match {
    case Left(a) => Left(a)
    case Right(b) => Right(b + 1)
  }))

  val prop_map2 = property((e1: Either[Int, Int], e2: Either[Int, Int]) =>
    e1.map2(e2, _ + (_: Int)) == (e1 match {
      case Left(a) => Left(a)
      case Right(b) => e2 match {
        case Left(aa) => Left(aa)
        case Right(bb) => Right(b + bb)
      }
    }))

  // ||>
  val prop_fmap2 = property((e1: Either[Int, Int], e2: Either[Int, Int]) =>
    e1.map2(e2, _ + (_: Int)) == e1.||>(e2, _ + (_: Int)))

  val prop_map3 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int]) =>
    e1.map3(e2, e3, _ + (_: Int) + (_: Int)) == (e1 match {
      case Left(a) => Left(a)
      case Right(b) => e2 match {
        case Left(aa) => Left(aa)
        case Right(bb) => e3 match {
          case Left(aaa) => Left(aaa)
          case Right(bbb) => Right(b + bb + bbb)
        }
      }
    }))

  // |||>
  val prop_fmap3 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int]) =>
    e1.map3(e2, e3, _ + (_: Int) + (_: Int)) == e1.|||>(e2, e3, _ + (_: Int) + (_: Int)))

  val prop_map4 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int]) =>
    e1.map4(e2, e3, e4, _ + (_: Int) + (_: Int) + (_: Int)) == (e1 match {
      case Left(a) => Left(a)
      case Right(b) => e2 match {
        case Left(aa) => Left(aa)
        case Right(bb) => e3 match {
          case Left(aaa) => Left(aaa)
          case Right(bbb) => e4 match {
            case Left(aaaa) => Left(aaaa)
            case Right(bbbb) => Right(b + bb + bbb + bbbb)
          }
        }
      }
    }))

  // ||||>
  val prop_fmap4 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int]) =>
    e1.map4(e2, e3, e4, _ + (_: Int) + (_: Int) + (_: Int)) == e1.||||>(e2, e3, e4, _ + (_: Int) + (_: Int) + (_: Int)))

  val prop_map5 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int], e5: Either[Int, Int]) =>
    e1.map5(e2, e3, e4, e5, _ + (_: Int) + (_: Int) + (_: Int) + (_: Int)) == (e1 match {
      case Left(a) => Left(a)
      case Right(b) => e2 match {
        case Left(aa) => Left(aa)
        case Right(bb) => e3 match {
          case Left(aaa) => Left(aaa)
          case Right(bbb) => e4 match {
            case Left(aaaa) => Left(aaaa)
            case Right(bbbb) => e5 match {
              case Left(aaaaa) => Left(aaaaa)
              case Right(bbbbb) => Right(b + bb + bbb + bbbb + bbbbb)
            }
          }
        }
      }
    }))

  // |||||>
  val prop_fmap5 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int], e5: Either[Int, Int]) =>
    e1.map5(e2, e3, e4, e5, _ + (_: Int) + (_: Int) + (_: Int) + (_: Int)) == e1.|||||>(e2, e3, e4, e5, _ + (_: Int) + (_: Int) + (_: Int) + (_: Int)))

  val prop_mapLeft = property((e: Either[Int, Int]) => (e mapLeft (_ + 1)) == (e match {
    case Left(a) => Left(a + 1)
    case Right(b) => Right(b)
  }))

  // <|
  val prop_fmapLeft = property((e: Either[Int, Int]) => (e <| (_ + 1)) == (e match {
    case Left(a) => Left(a + 1)
    case Right(b) => Right(b)
  }))

  val prop_mapLeft2 = property((e1: Either[Int, Int], e2: Either[Int, Int]) =>
    e1.mapLeft2(e2, _ + (_: Int)) == (e1 match {
      case Left(a) => e2 match {
        case Left(aa) => Left(a + aa)
        case Right(bb) => Right(bb)
      }
      case Right(b) => Right(b)
    }))

  // <||
  val prop_fmapLeft2 = property((e1: Either[Int, Int], e2: Either[Int, Int]) =>
    e1.mapLeft2(e2, _ + (_: Int)) == e1.<||(e2, _ + (_: Int)))

  val prop_mapLeft3 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int]) =>
    e1.mapLeft3(e2, e3, _ + (_: Int) + (_: Int)) == (e1 match {
      case Left(a) => e2 match {
        case Left(aa) => e3 match {
          case Left(aaa) => Left(a + aa + aaa)
          case Right(bbb) => Right(bbb)
        }
        case Right(bb) => Right(bb)
      }
      case Right(b) => Right(b)
    }))

  // <|||
  val prop_fmapLeft3 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int]) =>
    e1.mapLeft3(e2, e3, _ + (_: Int) + (_: Int)) == e1.<|||(e2, e3, _ + (_: Int) + (_: Int)))

  val prop_mapLeft4 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int]) =>
    e1.mapLeft4(e2, e3, e4, _ + (_: Int) + (_: Int) + (_: Int)) == (e1 match {
      case Left(a) => e2 match {
        case Left(aa) => e3 match {
          case Left(aaa) => e4 match {
            case Left(aaaa) => Left(a + aa + aaa + aaaa)
            case Right(bbbb) => Right(bbbb)
          }
          case Right(bbb) => Right(bbb)
        }
        case Right(bb) => Right(bb)
      }
      case Right(b) => Right(b)
    }))

  // <|||
  val prop_fmapLeft4 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int]) =>
    e1.mapLeft4(e2, e3, e4, _ + (_: Int) + (_: Int) + (_: Int)) == e1.<||||(e2, e3, e4, _ + (_: Int) + (_: Int) + (_: Int)))

  val prop_mapLeft5 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int], e5: Either[Int, Int]) =>
    e1.mapLeft5(e2, e3, e4, e5, _ + (_: Int) + (_: Int) + (_: Int) + (_: Int)) == (e1 match {
      case Left(a) => e2 match {
        case Left(aa) => e3 match {
          case Left(aaa) => e4 match {
            case Left(aaaa) => e5 match {
              case Left(aaaaa) => Left(a + aa + aaa + aaaa + aaaaa)
              case Right(bbbbb) => Right(bbbbb)
            }
            case Right(bbbb) => Right(bbbb)
          }
          case Right(bbb) => Right(bbb)
        }
        case Right(bb) => Right(bb)
      }
      case Right(b) => Right(b)
    }))

  // <|||
  val prop_fmapLeft5 = property((e1: Either[Int, Int], e2: Either[Int, Int], e3: Either[Int, Int], e4: Either[Int, Int], e5: Either[Int, Int]) =>
    e1.mapLeft5(e2, e3, e4, e5, _ + (_: Int) + (_: Int) + (_: Int) + (_: Int)) == e1.<|||||(e2, e3, e4, e5, _ + (_: Int) + (_: Int) + (_: Int) + (_: Int)))

  val prop_filter = property((e: Either[Int, Int], x: Int) => e.filter(_ % 2 == 0) ==
    (if(e.isLeft || e.right % 2 != 0) None else Some(e)))

  val prop_filter_ = property((e: Either[Int, Int], x: Int) => e.filter(_ % 2 == 0, x) ==
    (if(e.isLeft || e.right % 2 != 0) Left(x) else e))

  val prop_filterLeft = property((e: Either[Int, Int], x: Int) => e.filterLeft(_ % 2 == 0, x) ==
    (if(e.isRight || e.left % 2 != 0) Right(x) else e))

  val prop_mapIf = property((e: Either[Int, Int], x: Int) => e.mapIf(_ % 2 == 0, _ + 1) ==
    (if(e.isLeft || e.right % 2 != 0) e else Left(e.right + 1)))

  val prop_mapIfLeft = property((e: Either[Int, Int], x: Int) => e.mapIfLeft(_ % 2 == 0, _ + 1) ==
    (if(e.isRight || e.left % 2 != 0) e else Right(e.left + 1)))

  val prop_ap = property((e1: Either[Int, Int], e2: Either[Int, Int]) => {
    val ee2 = e2 map (n => (x: Int) => x + n)
    e1.ap(ee2) == (ee2 match {
      case Left(a) => Left(a)
      case Right(f) => e1 match {
        case Left(a) => Left(a)
        case Right(b) => Right(f(b))
      }
    })
  })

  val prop_apLeft = property((e1: Either[Int, Int], e2: Either[Int, Int]) => {
    val ee2 = e2 mapLeft (n => (x: Int) => x + n)
    e1.apLeft(ee2) == (ee2 match {
      case Right(b) => Right(b)
      case Left(f) => e1 match {
        case Right(b) => Right(b)
        case Left(a) => Left(f(a))
      }
    })
  })

  val prop_seq = property((e: Either[Int, Int]) => e.seq == (e match {
    case Left(_) => Seq.empty
    case Right(b) => Seq.singleton(b)
  }))

  val prop_seqLeft = property((e: Either[Int, Int]) => e.seqLeft == (e match {
    case Left(a) => Seq.singleton(a)
    case Right(_) => Seq.empty
  }))

  val prop_option = property((e: Either[Int, Int]) => e.option == (e match {
    case Left(_) => None
    case Right(b) => Some(b)
  }))

  val prop_optionLeft = property((e: Either[Int, Int]) => e.optionLeft == (e match {
    case Left(a) => Some(a)
    case Right(_) => None
  }))

  val prop_Either_left = property((n: Int) => Either.left(n).left == n)

  val prop_Either_right = property((n: Int) => Either.right(n).right == n)

  val prop_Either_join = property((e: Either[Int, Either[Int, Int]]) => e match {
    case Left(n) => Either.join(e) == Left(n)
    case Right(ee) => Either.join(e) == ee
  })

  val prop_Either_joinLeft = property((e: Either[Either[Int, Int], Int]) => e match {
    case Left(ee) => Either.joinLeft(e) == ee
    case Right(n) => Either.joinLeft(e) == Right(n)
  })

  val prop_Either_lefts = property((es: List[Either[Int, Int]]) =>
    Either.lefts(es) == es.filter(_.isLeft).map(_.left))

  val prop_Either_rights = property((es: List[Either[Int, Int]]) =>
    Either.rights(es) == es.filter(_.isRight).map(_.right))

  val prop_Either_leftRights = property((es: List[Either[Int, Int]]) =>
    Either.rights(es) == es.filter(_.isRight).map(_.right))

  val prop_Either_throws = property((n: Int) =>
    Either.throws(n) == Right(n) && Either.throws(error("error")).isLeft)

  val prop_Either_throwIt = property((e: Either[Throwable, Int]) =>
    try {
      Either.throwIt(e) == e.right
    } catch {
      case (t) => e.isLeft && e.left == t
    })

  val prop_Either_reduce = property((e: Either[Int, Int]) =>
    Either.reduce(e) == (e match {
      case Left(a) => a
      case Right(a) => a
    }))

  val prop_Either_iif = property((c: Boolean, a: Int, b: Int) =>
    Either.iif(c)(a, b) == (if(c) Right(b) else Left(a)))

  val tests = List(
      ("prop_leftE", prop_leftE),
      ("prop_rightE", prop_rightE),
      ("prop_left", prop_left),
      ("prop_right", prop_right),
      ("prop_swap", prop_swap),
      ("prop_isLeft", prop_isLeft),
      ("prop_isRight", prop_isRight),
      ("prop_either1", prop_either1),
      ("prop_either2", prop_either2),
      ("prop_ifLeft", prop_ifLeft),
      ("prop_ifRight", prop_ifRight),
      ("prop_foreach", prop_foreach),
      ("prop_if", prop_if),
      ("prop_left_", prop_left_),
      ("prop_right_", prop_right_),
      ("prop_leftOr", prop_leftOr),
      ("prop_rightOr", prop_rightOr),
      ("prop_flatMap", prop_flatMap),
      ("prop_bind", prop_bind),
      ("prop_anonymousBind", prop_anonymousBind),
      ("prop_flatMapLeft", prop_flatMapLeft),
      ("prop_bindLeft", prop_bindLeft),
      ("prop_anonymousBindLeft", prop_anonymousBindLeft),
      ("prop_mapLeftRight", prop_mapLeftRight),
      ("prop_map", prop_map),
      ("prop_fmap", prop_fmap),
      ("prop_map2", prop_map2),
      ("prop_fmap2", prop_fmap2),
      ("prop_map3", prop_map3),
      ("prop_fmap3", prop_map3),
      ("prop_map4", prop_map4),
      ("prop_fmap4", prop_fmap4),
      ("prop_map5", prop_map5),
      ("prop_fmap5", prop_fmap5),
      ("prop_mapLeft", prop_mapLeft),
      ("prop_fmapLeft", prop_fmapLeft),
      ("prop_mapLeft2", prop_mapLeft2),
      ("prop_fmapLeft2", prop_mapLeft2),
      ("prop_mapLeft3", prop_mapLeft3),
      ("prop_fmapLeft3", prop_mapLeft3),
      ("prop_mapLeft4", prop_mapLeft4),
      ("prop_fmapLeft4", prop_mapLeft4),
      ("prop_mapLeft5", prop_mapLeft5),
      ("prop_fmapLeft5", prop_mapLeft5),
      ("prop_filter", prop_filter),
      ("prop_filter_", prop_filter_),
      ("prop_filterLeft", prop_filterLeft),
      ("prop_mapIf", prop_mapIf),
      ("prop_mapIfLeft", prop_mapIfLeft),
      ("prop_ap", prop_ap),
      ("prop_apLeft", prop_apLeft),
      ("prop_seq", prop_seq),
      ("prop_seqLeft", prop_seqLeft),
      ("prop_option", prop_option),
      ("prop_optionLeft", prop_optionLeft),
      ("prop_Either_left", prop_Either_left),
      ("prop_Either_right", prop_Either_right),
      ("prop_Either_join", prop_Either_join),
      ("prop_Either_joinLeft", prop_Either_joinLeft),
      ("prop_Either_lefts", prop_Either_lefts),
      ("prop_Either_rights", prop_Either_rights),
      ("prop_Either_leftRights", prop_Either_leftRights),
      ("prop_Either_throws", prop_Either_throws),
      ("prop_Either_throwIt", prop_Either_throwIt),
      ("prop_Either_reduce", prop_Either_reduce),
      ("prop_Either_iif", prop_Either_iif)
    )

  def main(args: Array[String]) =
    tests foreach (tupled((name, prop) =>
    testStatsEx(name, testReport(check(Params(500, 0, 0, 500, StdRand), prop, propReport)))))
}
*/
