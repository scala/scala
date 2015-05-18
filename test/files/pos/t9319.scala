object Test {

  // was causing "filename too long" errors under -Ydelambdafy:method
  def main(args: Array[String]) {
    val x = List(1)
    (i: Int) => {
      (i: Int) => {
        (i: Int) => {
          (i: Int) => {
            (i: Int) => {
              (i: Int) => {
                (i: Int) => {
                  (i: Int) => {
                    (i: Int) => {
                      ()
                    }

                    ""
                  }
                  ()
                }
                ()
              }
              ()
            }
            ()
          }
          ()
        }
        ()
      }
      ()
    }
  }
}
