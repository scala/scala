trait SI_5054_q5 {
  /**
    * A simple comment
    * 
    * @param lost a lost parameter
    * @usecase def test(): Int
    */
   def test(implicit lost: Int): Int = lost
}
