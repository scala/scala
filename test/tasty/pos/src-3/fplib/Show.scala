package fplib

object allsyntax extends Show.Api // must be in a separate tasty file to Show

trait Show extends Show.Impl

object Show {

  trait Impl

  trait Api

}
