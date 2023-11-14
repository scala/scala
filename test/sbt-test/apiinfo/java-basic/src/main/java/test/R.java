package test;

public final class R {
   public static final int y = 4;
   public static int x = (new stest.S()).y();
	public static void main(String[] args)
	{
		assert(args[0] == "1");
	}
}
