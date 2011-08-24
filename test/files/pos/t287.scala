object testBuf {
  class mystream extends java.io.BufferedOutputStream(new java.io.FileOutputStream("/dev/null")) {
    def w( x:String ):Unit = {
      val foo = new Array[Byte](2);

      // write( byte[] ) is defined in FilterOutputStream, the superclass of BufferedOutputStream
      super.write( foo ); // error

      super.write( foo, 0, foo.length ); // this works however
    }
  }
}
