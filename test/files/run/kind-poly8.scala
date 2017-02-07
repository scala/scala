import scala.language.higherKinds

object Test extends App {


  trait ShowType[T <: AnyKind] {
    def show: String
  }

  object ShowType {

    def apply[T <: AnyKind](implicit st: ShowType[T]): st.type = st
    
    implicit def show1A[F[_], A](implicit stf: ShowType[F], sta: ShowType[A]) =
      new ShowType[F[A]] { def show = s"${stf.show}[${sta.show}]" }

    implicit def show2AB[F[_, _], A, B](implicit stf: ShowType[F], sta: ShowType[A], stb: ShowType[B]) =
      new ShowType[F[A, B]] { def show = s"${stf.show}[${sta.show}, ${stb.show}]" }

    implicit def showInt = new ShowType[Int] { def show = s"Int" }
    implicit def showString = new ShowType[String] { def show = s"String" }

    implicit def showList = new ShowType[List] { def show = s"List" }
    implicit def showMap = new ShowType[Map] { def show = s"Map" }
    
  }


  assert(ShowType[Int].show == "Int")
  assert(ShowType[List].show == "List")
  assert(ShowType[List[Int]].show == "List[Int]")

  assert(ShowType[Map].show == "Map")
  assert(ShowType[Map[Int, String]].show == "Map[Int, String]")

  trait HeadOption[T <: AnyKind] {
    import HeadOption._
    def headOption[TA, A](ta: TA)(implicit unap: UnapplyApply[TA, T, A, this.type]): Option[A] = unap.apply(ta)
  }

  object HeadOption {
    def apply[T <: AnyKind](implicit ho: HeadOption[T]): ho.type = ho

    trait UnapplyApply[TA, T <: AnyKind, A, M <: HeadOption[T]] {
      def apply(ta: TA): Option[A]
    }

    implicit def listUnap[A, M <: HeadOption[List]] = new HeadOption.UnapplyApply[List[A], List, A, M] {
      def apply(ta: List[A]): Option[A] = ta.headOption
    }
    implicit def listHO = new HeadOption[List] {}

    implicit def mapUnap[A, B, M <: HeadOption[Map]] = new HeadOption.UnapplyApply[Map[A, B], Map, (A, B), M] {
      def apply(ta: Map[A, B]): Option[(A, B)] = ta.headOption
    }

    implicit def mapHO = new HeadOption[Map] {}
  }

  assert(HeadOption[List].headOption(List(5, 6, 7)) == Some(5))
  assert(HeadOption[Map].headOption(Map("toto" -> 5L, "tata" -> 10L)) == Some("toto" -> 5L))
}


