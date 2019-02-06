package scala

import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.Prop.AnyOperators

/**
 * Property tests for code in [[scala.Option]]'s documentation.
 */
object OptionTest extends Properties("scala.Option") {

  property("map") = {
    Prop.forAll { (option: Option[Int], i: Int) =>
      val f: Function1[Int,Int] = (_ => i)
      option.map(f(_)) ?= {
        option match {
          case Some(x) => Some(f(x))
          case None    => None
        }
      }
    }
  }

  property("flatMap") = {
    Prop.forAll { (option: Option[Int], i: Int) =>
      val f: Function1[Int,Option[Int]] = (_ => Some(i))
      option.flatMap(f(_)) ?= {
        option match {
          case Some(x) => f(x)
          case None    => None
        }
      }
    }
  }

  property("foreach") = {
    Prop.forAll { (option: Option[Int], unit: Unit) =>
      val proc: Function1[Int,Unit] = (_ => unit)
      option.foreach(proc(_)) ?= {
        option match {
          case Some(x) => proc(x)
          case None    => ()
        }
      }
    }
  }

  property("fold") = {
    Prop.forAll { (option: Option[Int], i: Int, y: Int) =>
      val f: Function1[Int,Int] = (_ => i)
      option.fold(y)(f(_)) ?= {
        option match {
          case Some(x) => f(x)
          case None    => y
        }
      }
    }
  }

  property("foldLeft") = {
    Prop.forAll { (option: Option[Int], i: Int, y: Int) =>
      val f: Function2[Int,Int,Int] = ((_, _) => i)
      option.foldLeft(y)(f(_, _)) ?= {
        option match {
          case Some(x) => f(y, x)
          case None    => y
        }
      }
    }
  }

  property("foldRight") = {
    Prop.forAll { (option: Option[Int], i: Int, y: Int) =>
      val f: Function2[Int,Int,Int] = ((_, _) => i)
      option.foldRight(y)(f(_, _)) ?= {
        option match {
          case Some(x) => f(x, y)
          case None    => y
        }
      }
    }
  }

  property("collect") = {
    Prop.forAll { (option: Option[Int], i: Int) =>
      val pf: PartialFunction[Int,Int] = {
        case x if x > 0 => i
      }
      option.collect(pf) ?= {
        option match {
          case Some(x) if pf.isDefinedAt(x) => Some(pf(x))
          case _                            => None
        }
      }
    }
  }

  property("isDefined") = {
    Prop.forAll { option: Option[Int] =>
      option.isDefined ?= {
        option match {
          case Some(_) => true
          case None    => false
        }
      }
    }
  }

  property("isEmpty") = {
    Prop.forAll { option: Option[Int] =>
      option.isEmpty ?= {
        option match {
          case Some(_) => false
          case None    => true
        }
      }
    }
  }

  property("nonEmpty") = {
    Prop.forAll { option: Option[Int] =>
      option.nonEmpty ?= {
        option match {
          case Some(_) => true
          case None    => false
        }
      }
    }
  }

  property("orElse") = {
    Prop.forAll { (option: Option[Int], y: Option[Int]) =>
      option.orElse(y) ?= {
        option match {
          case Some(x) => Some(x)
          case None    => y
        }
      }
    }
  }

  property("getOrElse") = {
    Prop.forAll { (option: Option[Int], y: Int) =>
      option.getOrElse(y) ?= {
        option match {
          case Some(x) => x
          case None    => y
        }
      }
    }
  }

  property("get") = {
    Prop.forAll { (option: Option[Int]) =>
      Prop.iff[Option[Int]](option, {
        case Some(x) =>
          option.get ?= {
            option match {
              case Some(x) => x
              case None    => throw new Exception
            }
          }
        case None =>
          Prop.throws(classOf[Exception]) {
            option.get
          }
      })
    }
  }

  property("orNull") = {
    Prop.forAll { (option: Option[String]) =>
      option.orNull ?= {
        option match {
          case Some(s) => s
          case None    => null
        }
      }
    }
  }

  property("filter") = {
    Prop.forAll { (option: Option[Int], bool: Boolean) =>
      val pred: Function1[Int,Boolean] = (_ => bool)
      option.filter(pred(_)) ?= {
        option match {
          case Some(x) if pred(x) => Some(x)
          case _                  => None
        }
      }
    }
  }

  property("filterNot") = {
    Prop.forAll { (option: Option[Int], bool: Boolean) =>
      val pred: Function1[Int,Boolean] = (_ => bool)
      option.filterNot(pred(_)) ?= {
        option match {
          case Some(x) if !pred(x) => Some(x)
          case _                   => None
        }
      }
    }
  }

  property("exists") = {
    Prop.forAll { (option: Option[Int], bool: Boolean) =>
      val pred: Function1[Int,Boolean] = (_ => bool)
      option.exists(pred(_)) ?= {
        option match {
          case Some(x) => pred(x)
          case None    => false
        }
      }
    }
  }

  property("forall") = {
    Prop.forAll { (option: Option[Int], bool: Boolean) =>
      val pred: Function1[Int,Boolean] = (_ => bool)
      option.forall(pred(_)) ?= {
        option match {
          case Some(x) => pred(x)
          case None    => true
        }
      }
    }
  }

  property("contains") = {
    Prop.forAll { (option: Option[Int], y: Int) =>
      option.contains(y) ?= {
        option match {
          case Some(x) => x == y
          case None    => false
        }
      }
    }
  }

  property("size") = {
    Prop.forAll { option: Option[Int] =>
      option.size ?= {
        option match {
          case Some(x) => 1
          case None    => 0
        }
      }
    }
  }

  property("toList") = {
    Prop.forAll { option: Option[Int] =>
      option.toList ?= {
        option match {
          case Some(x) => List(x)
          case None    => Nil
        }
      }
    }
  }

  property("toRight") = {
    Prop.forAll { (option: Option[Int], i: Int) =>
      option.toRight(i) ?= {
        option match {
          case Some(x) => scala.util.Right(x)
          case None    => scala.util.Left(i)
        }
      }
    }
  }

  property("toLeft") = {
    Prop.forAll { (option: Option[Int], i: Int) =>
      option.toLeft(i) ?= {
        option match {
          case Some(x) => scala.util.Left(x)
          case None    => scala.util.Right(i)
        }
      }
    }
  }
}
