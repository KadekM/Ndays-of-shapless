# DAY 1

Let's try the most basic primitive, HList:
```scala
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
```scala
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

```scala
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

val ys = 1 :: (2 :: ("abc" :: HNil) :: HNil) :: HNil
val ysm = ys.map(size)
println(ysm) // 1 :: 2 :: 3 :: HNil :: HNil :: HNil
```

It's not really flattened - it's just the way it's displayed it seems.

```scala
println(1 :: (2 :: HNil) :: ("a" :: HNil) :: HNil)
//1 :: 2 :: HNil :: a :: HNil :: HNil
```
Indeed, it seems to be the case.

How about function which maps everything to constant value?
```scala
val xs = 32 :: "abc" :: HNil

object const extends (Id ~> Const[String]#λ) {
  def apply[T](f: Id[T]): Const[String]#λ[T] = "c"
}

val xsm : String :: String :: HNil = xs.map(const)
println(xsm) // c :: c :: HNil
```

There seem to be a function for that.
```scala
val xsm2 : String :: String :: HNil = xs.mapConst("c")
println(xsm2) // c :: c :: HNil
```

How about HList, lifting it to HList of Successes, transforming to HList of Options, and extracting values from options?

```scala
val xs = 1 :: "some" :: HNil

object lift extends (Id ~> Try) {
  override def apply[T](f: Id[T]): Try[T] = Success(f)
}
val xsl: Try[Int] :: Try[String] :: HNil = xs.map(lift)

object change extends (Try ~> Option) {
  override def apply[T](f: Try[T]): Option[T] = f.toOption
}
val xso: Option[Int] :: Option[String] :: HNil = xsl.map(change)

object extract extends (Option ~> Id) {
  override def apply[T](f: Option[T]): Id[T] = f.get
}
val xse: Int :: String :: HNil = xso.map(extract)

println(xse) // 1 :: some :: HNil
```

How about if we want to pass poly function to our arbitrary def?
```scala
val xs = 1 :: "some" :: HNil

object size extends Poly1 {
  implicit def caseInt = at[Int](x ⇒ x)
  implicit def caseString = at[String](_.length)
}

def fn[L <: HList](ss: L)(p: Poly1) =
  ss.map(p)

val xsm = fn(xs)(size)
println(xsm) // 1 :: 4 :: HNil
```
Fails at:
```
[error] could not find implicit value for parameter mapper: shapeless.ops.hlist.Mapper[p.type,L]
[error]       ss.map(p)
```

Let's supply him the Mapper then.
```scala
def fn[L <: HList](ss: L)(p: Poly1)
	(implicit m: Mapper[p.type, L]): m.Out =
  ss.map(p)

val xsm = fn(xs)(size)
println(xsm) // 1 :: 4 :: HNil
```
Perfect.

Let's try calling same map twice. Result should be same though, since calling Poly1 on 1 :: 4 results in 1 :: 4.
```scala
def fn[L <: HList](ss: L)(p: Poly1)
	(implicit m: Mapper[p.type, L]): m.Out =
  ss.map(p).map(p)
```

Well, damn! Compile error:
```
[error] could not find implicit value for parameter mapper: shapeless.ops.hlist.Mapper[p.type,m.Out]
[error]       ss.map(p).map(p)
```
Result of first map is m.Out. We need to extract the type level computation result of map. Let's use Aux.

```scala
def fn[L <: HList, R <: HList](ss: L)(p: Poly1)
	(implicit m1: Mapper.Aux[p.type, L, R],
	  m2: Mapper[p.type, R]) =
         ss.map(p).map(p)

val xsm = fn(xs)(size)
println(xsm) // 1 :: 4 :: HNil
```

All seems fine. Let's change Poly1 so we have something observable.
```scala
val xs = 1 :: "some" :: HNil

object size extends Poly1 {
  implicit def caseInt = at[Int](x ⇒ 1)
  implicit def caseString = at[String](_.length)
}

def fn[L <: HList, R <: HList](ss: L)(p: Poly1)
	(implicit m1: Mapper.Aux[p.type, L, R],
	  m2: Mapper[p.type, R]) =
         ss.map(p).map(p)

val xsm = fn(xs)(size)
println(xsm) // 1 :: 1 :: HNil
```

First map transforms xs into  `1 :: 4 :: HNil`, second map into `1 :: 1 :: HNil`. Great.

Let's make a fold that counts size of whole HList, and returns it in type of last element.

```scala
val xs = 1 :: "other" :: 2 :: HNil
val ys = 1 :: "other" :: "ye" :: HNil

object fold extends Poly2 {
  implicit def caseIntOnInt = at[Int, Int]
    { case (z, x) => z + x }
  implicit def caseStringOnInt = at[Int, String]
    { case (z, x) => (z + x.length).toString }
  implicit def caseIntOnString = at[String, Int]
    { case (z, x) => z.toInt + x }
  implicit def caseStringOnString = at[String, String]
    { case (z, x) => (z.toInt + x.length).toString }
}

val xsm = xs.foldLeft(0)(fold)
println(xsm, xsm.getClass) // 8, int

val ysm = ys.foldLeft(0)(fold)
println(ysm, ysm.getClass) // 8, class java.lang.String
```

Lastly, let's try to make a function that takes a HList, a reverse version of same HList, and zips them together.
```scala
val xs = 1 :: "other" :: 1.23 :: HNil
val xsr = xs.reverse

val xsr2 = 1.23 :: "other" :: 1 :: HNil

def fn[L <: HList, R <: HList](list: L, listRev: R)
  (implicit ev: Reverse.Aux[L, R],
             z: Zip[L :: R :: HNil]) =
    list.zip(listRev)

val xsm = fn(xs, xsr)
val xsm2 = fn(xs, xsr2)
println(xsm) //(1,1.23) :: (other,other) :: (1.23,1) :: HNil
println(xsm2) //(1,1.23) :: (other,other) :: (1.23,1) :: HNil

val xsrInvalid1 = xs
val xsrInvalid2 = xsr :: "add" :: HNil
//fn(xs, xsrInvalid1) // does not compile
//fn(xs, xsrInvalid2) // does not compile
```
