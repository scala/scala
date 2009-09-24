object Test
{
    def f[T](recurse: T => List[T]): List[T] =
    {
        Nil
    }

    abstract class M
    { self =>
    	type Settings
    	type selfType = M {type Settings = self.Settings}

        val v: List[selfType] = f[selfType]((x: selfType) => x.v)
    }

    abstract class M2
    { self =>
    	type Settings
    	type selfType = M2 {type Settings = self.Settings}

        def g: List[selfType] = Nil

        {
			f[selfType](_.g)
        }
    }
}

