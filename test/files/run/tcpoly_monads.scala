trait Monads {
  /**
   * class Monad m where
   *   (>>=)  :: m a -> (a -> m b) -> m b
   *   return :: a -> m a
   *
   * MonadTC encodes the above Haskell type class, 
   * an instance of MonadTC corresponds to a method dictionary.
   * (see http://lampwww.epfl.ch/~odersky/talks/wg2.8-boston06.pdf)
   *
   * Note that the identity (`this') of the method dictionary does not really correspond
   * to the instance of m[x] (`self') that is `wrapped': e.g., unit does not use `self' (which 
   * corresponds to the argument of the implicit conversion that encodes an instance of this type class)
   */
  trait MonadTC[m[x], a] {      
    def unit[a](orig: a): m[a]

    // >>='s first argument comes from the implicit definition constructing this "method dictionary"
    def >>=[b](fun: a => m[b]): m[b]
  }
}

/**
 * instance Monad Maybe where
 *   (Just x) >>= k = k x
 *   Nothing  >>= _ = Nothing
 */
trait OptionMonad extends Monads {
  // this implicit method encodes the Monad type class instance for Option
  implicit def OptionInstOfMonad[a](self: Option[a]): MonadTC[Option, a] 
    = new MonadTC[Option, a] {
        def unit[a](orig: a) = Some(orig)
        def >>=[b](fun: a => Option[b]): Option[b] = self match {
          case Some(x) => fun(x)
          case None => None
        }
      }
}

object Test extends OptionMonad with App {
  Console.println((Some("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") >>= (x => Some(x.length))).get)
}
