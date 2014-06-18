package scala.collection.immutable

import language.{higherKinds, implicitConversions}

/**
 * @author Steven Dobay
 * @version 1.0
 *
 * HList(Heterogenous List) isan immutable data-strucutre to store
 * elems with different types without castin those to AnyRef or Object. It constructed by case classes: HCons(VALUE, HList) and HNil()
 * Constructing a HList:
 * {{{
 * val hl = 1 ::: true ::: "a string" ::: HNil()
 * }}}
 * Simple methods:
 * {{{
 * hl.size // yields 3
 * hl.head // yields 1
 * hl.tail // yields HCons(true, HCons("a string", HNil()))
 * hl ++ (0 ::: false) //yields HCons(1, HCons(true, HCons("a string", HCons(0, HCons(false, HNil())))))
 * }}}
 * 
 * It can be converted to tuples from 2 to 8:
 * {{{
 * hl.tuple3[Int, Boolean, String] // yields Some((1, true, "a string"))
 * hl.tuple2[Int, Boolean] // yields Some((1, true))
 * hl.tuple4[Int, Boolean, String, Int] // yields None, if the size of HList is lesser than the expected size of tuple
 * }}}
 */
sealed trait HList
 
final case class HCons[+H, +T <: HList](head: H, tail: T) extends HList {
   def :::[T](v: T) = HCons(v, this)
}
 
sealed case class HNil() extends HList {
   def :::[T](v: T) = HCons(v, this)
}
 
object HList {
  type :::[+H, +T <: HList] = HCons[H, T]
  val ::: = HCons
  
  final implicit class listToHList[A](l: List[A]){
    def toHList: HList = {
        def rec(hb: HList, lh: List[A]): HList =
            if(lh.size == 0) hb
            else rec(HCons(lh.head, hb), lh.tail)
        rec(HNil(), l).reverse
    }      
 }

 final implicit class HListOps(h: HList){
  def head: Option[Any] = 
      h match{
        case HCons(x, _) => Some(x)
        case _           => None
      }
      
  def tail: HList = 
      h match{
        case HCons(_, x) => x
        case _           =>HNil()
      }
      
  def init: HList =
      h.take(h.size - 1)
  
  def str[A]: String =
      "HList(" + h.toList[A].mkString(", ") + ")"
      
  def last[A]: A = 
      h.drop(h.size - 1).head.get.asInstanceOf[A]
      
  def append(b: HList): HList = {
      def add1(hb: HList, hr: HList): HList =
          hb match{
             case HCons(hd, tl) => add1(tl, HCons(hd, hr))
             case _             => hr
          }
      def add2(hb: HList, hr: HList): HList = 
          hb match{
             case HCons(hd, tl) => add2(tl, HCons(hd, hr))
             case _             => hr
          }
      add2(add1(h, HNil()), b)
  }
  
  def ++(b: HList): HList = h.append(b)
      
   def size: Int = {
       def rec(hb: HList, n: Int): Int =
           hb match{
              case HCons(hd, tl) => rec(tl, n + 1)
              case _             => n
           }
       rec(h, 0)
   }
   
   def map[A, B](f: A => B): HList = {
       def rec(hb: HList, hr: HList): HList = 
           hb match{
              case HCons(hd, tl) => rec(tl, HCons(f(hd.asInstanceOf[A]), hr))
              case _             => hr
           }
       rec(h, HNil()).reverse
   }
   
   def reverse: HList = {
       def rev(hb: HList, hr: HList): HList = 
           hb match{
              case HCons(hd, tl) => rev(tl, HCons(hd, hr))
              case _             => hr
           }
       rev(h, HNil())
   }
   
   def foldLeft[A](z: A)(f: (A, A) => A): A = {
       def rec(hb: HList, n: A): A =
           hb match{
              case HCons(hd, tl) => rec(tl, f(hd.asInstanceOf[A], n))
              case _             => n
           }
           if(h.size == 0) z else rec(h.tail, f(z, h.head.get.asInstanceOf[A]))
   }
   
   def take(n: Int): HList = {
        def rec(hb: HList, hr: HList, n: Int) : HList =
            hb match{
               case HCons(hd, tl) => if(n == 0) hr
                                     else rec(tl, HCons(hd, hr), n - 1)
               case _             => hr
            }
         rec(h, HNil(), n).reverse
   }
   
   def takeWhile[A](f: A => Boolean): HList = {
        def rec(hb: HList, hr: HList) : HList =
            hb match{
               case HCons(hd, tl) => if(f(hd.asInstanceOf[A])) rec(tl, HCons(hd, hr))
                                     else hr
               case _             => hr
            }
        rec(h, HNil()).reverse
   }
   
   def drop(n: Int): HList = {
       def rec(hb: HList, n: Int) : HList =
           hb match{
              case HCons(hd, tl) => if(n == 0) hb else rec(tl, n - 1)
              case _             => HNil()
           }
        rec(h, n)
   }
   
   def dropWhile[A](f: A => Boolean): HList = {
       def rec(hb: HList): HList = 
           hb match{
              case HCons(hd, tl) => if(f(hd.asInstanceOf[A])) rec(tl) else hb
              case _             => HNil()
           }
       rec(h)
   }
   
   def toList[A]: List[A] = {
       def rec(hb: HList, buffer: List[A]): List[A] =
           hb match{
              case HCons(hd, tl) => rec(tl, buffer ++ List(hd.asInstanceOf[A]))
              case _             => buffer
           }
       rec(h, List())
   }
   
   def toAnyList: List[Any] = {
       def rec(hb: HList, buffer: List[Any]): List[Any] =
           hb match{
              case HCons(hd, tl) => rec(tl, buffer ++ List(hd.asInstanceOf[Any]))
              case _             => buffer
           }
       rec(h, List())
   }
   
   def tuple2[A, B]: Option[Tuple2[A, B]] = 
       if(h.size < 2) None 
       else Some((h.head.get.asInstanceOf[A], h.tail.head.get.asInstanceOf[B]))
       
   def tuple3[A, B, C]: Option[Tuple3[A, B, C]] = 
       if(h.size < 3) None 
       else Some((h.head.get.asInstanceOf[A],
                  h.tail.head.get.asInstanceOf[B],
                  h.drop(2).head.get.asInstanceOf[C]))
                  
   def tuple4[A, B, C, D]: Option[Tuple4[A, B, C, D]] = 
       if(h.size < 4) None 
       else Some((h.head.get.asInstanceOf[A],
                  h.tail.head.get.asInstanceOf[B],
                  h.drop(2).head.get.asInstanceOf[C],
                  h.drop(3).head.get.asInstanceOf[D]))
                  
   def tuple5[A, B, C, D, E]: Option[Tuple5[A, B, C, D, E]] = 
       if(h.size < 5) None 
       else Some((h.head.get.asInstanceOf[A],
                  h.tail.head.get.asInstanceOf[B],
                  h.drop(2).head.get.asInstanceOf[C],
                  h.drop(3).head.get.asInstanceOf[D],
                  h.drop(4).head.get.asInstanceOf[E]))
                  
   def tuple6[A, B, C, D, E, F]: Option[Tuple6[A, B, C, D, E, F]] = 
       if(h.size < 6) None 
       else Some((h.head.get.asInstanceOf[A],
                  h.tail.head.get.asInstanceOf[B],
                  h.drop(2).head.get.asInstanceOf[C],
                  h.drop(3).head.get.asInstanceOf[D],
                  h.drop(4).head.get.asInstanceOf[E],
                  h.drop(5).head.get.asInstanceOf[F]))
                  
  def tuple7[A, B, C, D, E, F, G]: Option[Tuple7[A, B, C, D, E, F, G]] = 
      if(h.size < 7) None 
      else Some((h.head.get.asInstanceOf[A],
                  h.tail.head.get.asInstanceOf[B],
                  h.drop(2).head.get.asInstanceOf[C],
                  h.drop(3).head.get.asInstanceOf[D],
                  h.drop(4).head.get.asInstanceOf[E],
                  h.drop(5).head.get.asInstanceOf[F],
                  h.drop(6).head.get.asInstanceOf[G]))
                  
   def tuple8[A, B, C, D, E, F, G, H]: Option[Tuple8[A, B, C, D, E, F, G, H]] = 
      if(h.size < 8) None 
      else Some((h.head.get.asInstanceOf[A],
                  h.tail.head.get.asInstanceOf[B],
                  h.drop(2).head.get.asInstanceOf[C],
                  h.drop(3).head.get.asInstanceOf[D],
                  h.drop(4).head.get.asInstanceOf[E],
                  h.drop(5).head.get.asInstanceOf[F],
                  h.drop(6).head.get.asInstanceOf[G],
                  h.drop(7).head.get.asInstanceOf[H]))                                      
 }
}
