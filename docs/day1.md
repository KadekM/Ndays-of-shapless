# DAY 1

Let's try the most basic primitive, HList:
```
    val xs = 32 :: "test" :: 66 :: HNil

    object m extends Poly1 {
      implicit def caseString = at[String](_ + "mapped")
      implicit def caseInt = at[Int](_ + 100)
    }

    val xsm: Int :: String :: Int :: HNil = xs.map(m)
    println(xsm) // 132 :: testmapped :: 166 :: HNil
```

Everything as expected. In m we find cases for all types that are in xs.

Lets see if we want to do size, where size of Int is its value and size of
String is it's length. Should also work for tuple.
```
  val xs = 32 :: "test" :: (150, "abcd") :: HNil
    object size extends Poly1 {
      implicit def caseInt = at[Int](x => x)
      implicit def caseString = at[String](_.length)
      implicit def caseTuple[A, B](
        implicit ca: Case.Aux[A, Int],
        cb: Case.Aux[B, Int]) = at[(A, B)](x => ca(x._1) + cb(x._2))
    }

    val xsm: Int :: Int :: Int :: HNil = xs.map(size)
    println(xsm) // 32 :: 4 :: 154 :: HNil
```

This does not work for triple. Can we fix it for tuples of arbitrary size? And what about HLists inside HList. Let's first solve the HLists, and then we just convert tuples to HLists and apply same mechanism.

```
{
    val xs = 32 :: ("single", 12) :: "abc" :: (150 :: "abcd" :: "e" :: HNil) :: HNil

    object size extends Poly1 {
      implicit def caseInt = at[Int](x ⇒ x)
      implicit def caseString = at[String](_.length)

      implicit def caseHList[A <: HList](implicit m: Mapper[this.type, A]) = at[A](xs ⇒ xs.map(this))

      implicit def caseTuple[A <: Product, L <: HList](implicit g: Generic.Aux[A, L],
        m: Mapper[this.type, L]) =
        at[A](a ⇒ g.to(a).map(this))
      }

    val xsm = xs.map(size)
    println(xsm) // 32 :: 6 :: 12 :: HNil :: 3 :: 1 :: HNil :: HNil
  }
  ```
  It's not really flattened - it's just the way it's displayed it seems.

  ```
  println(1 :: (2 :: HNil) :: ("a" :: HNil) :: HNil)
  //1 :: 2 :: HNil :: a :: HNil :: HNil
  ```
  Indeed, it seems ok.


