package test

object TopLevelModule1
{
	object InnerModule1
	{
		object InnerModule2
		{
			trait Z { def q = 3 }
			def x = 3
		}
	}
	class InnerClass1
	{
		class InnerClass2
		{
			val z = new TopLevelModule1.InnerClass2
		}
		object InnerModule3
		{
			val y = new TopLevel1 with InnerModule1.InnerModule2.Z { val x = 4 }
		}
	}
	class InnerClass2
}
class TopLevel1
{
	object Inner1_1
}
object TopLevel1
{
	class Inner1_2
	object Inner1_2
}

object TopLevel2
class TopLevel2

object TopLevel3

class TopLevel4