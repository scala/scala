object test {

   def verifyKeyword(keyword : String, source : java.io.File, pos : Int) = {
      assert(keyword != null);
    }

    def verifyKeyword(source : java.io.File, pos : Int) =
      verifyKeyword("", source, pos);
}
