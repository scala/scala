object t4202_1 {
  () => {
    trait T {
      def t = ()
    } 
  }
}

object t4202_2 {
  () => {
    trait T {
      def t = ()
    }
    object T2 extends T {
      t
    }
  }
}
