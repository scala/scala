object Util {
  def mono(x: Int) = x
  def poly[T](x: T): T = x
}

trait FunSam[-T, +R] { def apply(x: T): R }


trait TFun { def map[T](f: T => Int): Unit = () }
object Fun extends TFun { import Util._
  def map[T: scala.reflect.ClassTag](f: T => Int): Unit = ()

  map(mono)
  map(mono _)
  map(x => mono(x))

// can't infer polymorphic type for function parameter:
//  map(poly)
//  map(poly _)
//  map(x => poly(x))
}

trait TSam { def map[T](f: T FunSam Int): Unit = () }
object Sam extends TSam { import Util._
  def map[T: scala.reflect.ClassTag](f: T `FunSam` Int): Unit = ()

  map(mono) // sam
  map(mono _) // sam
  map(x => mono(x)) // sam

// can't infer polymorphic type for function parameter:
//  map(poly)
//  map(poly _)
//  map(x => poly(x))
}

trait IntFun { def map[T](f: Int => T): Unit = () }
object int_Fun extends IntFun { import Util._
  def map[T: scala.reflect.ClassTag](f: Int => T): Unit = ()

  map(mono)
  map(mono _)
  map(x => mono(x))

  map(poly)
  map(poly _)
  map(x => poly(x))
}

trait IntSam { def map[T](f: Int FunSam T): Unit = () }
object int_Sam extends IntSam { import Util._
  def map[T: scala.reflect.ClassTag](f: Int `FunSam` T): Unit = ()

  map(mono) // sam
  map(mono _) // sam
  map(x => mono(x)) // sam

  map(poly) // sam
  map(poly _) // sam
  map(x => poly(x)) // sam
}


/*
eta_overload_hof.scala:27: error: missing argument list for method mono in object Util
Unapplied methods are only converted to functions when a function type is expected.
You can make this conversion explicit by writing `mono _` or `mono(_)` instead of `mono`.
  map(mono)
      ^
eta_overload_hof.scala:46: error: type mismatch;
 found   : Nothing => Nothing
 required: ?<: Int => ?
  map(poly _)
      ^
eta_overload_hof.scala:54: error: missing argument list for method mono in object Util
Unapplied methods are only converted to functions when a function type is expected.
You can make this conversion explicit by writing `mono _` or `mono(_)` instead of `mono`.
  map(mono)
      ^
eta_overload_hof.scala:58: error: missing argument list for method poly in object Util
Unapplied methods are only converted to functions when a function type is expected.
You can make this conversion explicit by writing `poly _` or `poly(_)` instead of `poly`.
  map(poly)
      ^
eta_overload_hof.scala:59: error: overloaded method value map with alternatives:
  [T](f: FunSam[Int,T])(implicit evidence$4: scala.reflect.ClassTag[T])Unit <and>
  [T](f: FunSam[Int,T])Unit
 cannot be applied to (Nothing => Nothing)
  map(poly _)
  ^
eta_overload_hof.scala:60: error: missing parameter type
  map(x => poly(x))
      ^

* */
