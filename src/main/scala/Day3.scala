import shapeless._
import shapeless.ops.hlist._

object Day3 extends App {
  {
    val t = (1, "test", 42, "brother")
    case class C(i: Int, n: String, ii: Int, nn: String)
    val c = C.apply(1, "test", 42, "brother")

    object m extends Poly1 {
      implicit def mapi = at[Int](i ⇒ i + i)
      implicit def maps = at[String](x ⇒ x + x)
    }

    import syntax.std.tuple._
    val tm = t.map(m)
    println(tm) // (2,testtest,84,brotherbrother)

    val cm = c.map(m)
    println(cm) // (2,testtest,84,brotherbrother)
  }

  {
    object foo {
      def bar: Int = 12345
    }
    def fn(x: foo.type) = println(x)

    fn(foo)
    //fn("something") // error: found String("something") required foo.type
    //fn(12345) // error: found Int(12345) required foo.type

    //12345.type // identifier expected but 'type' found
  }

  {
    import newtype._
    type UserId = Newtype[Int, Int]
    def UserId(i: Int): UserId = newtype(i)

    def getFromDb(id: UserId) = println(s"fetched $id")

    //getFromDb(12) // found Int(12) required UserId
    getFromDb(UserId(12)) // fetched 12
  }

  {
    import newtype._
    class User { // eww
      private var name = "user"
      def mutate() = name = name + name
    }

    type UserReadonly = Newtype[User, UserReadonlyOps]
    def UserReadonly(user: User): UserReadonly = newtype(user)
    case class UserReadonlyOps(private val u: User) {
    }
    implicit val ops = UserReadonlyOps

    val user = new User
    val readonly = UserReadonly(user)

    def readUserdata(u: UserReadonly) = {
      // u.mutate() // compile fails
      // u.user     // compile fails
      u.toString
    }

    //readUserdata(user) // compile fails
    readUserdata(readonly)
  }

  {
    import syntax.singleton._
    val w = Witness(12345)
    println(w) // shapeless.Witness$$anon$1@1fc7c5e8
    println(w.value) // 12345 - single inhabitant of the type

    def fn(x: w.T) = println(x)
    fn(12345) // 12345
    //fn(123456) // error: found Int(123456) required: w.T
  }

  {
    case class Cat()
    case class Dog()
    case class Parrot()

    val wcat = Witness("cat")
    val wdog = Witness("dog")
    val wpar = Witness("par")

    trait Select[T] { type Out }
    implicit val scat = new Select[wcat.T] { type Out = Cat }
    implicit val sdog = new Select[wdog.T] { type Out = Dog }
    implicit val spar = new Select[wpar.T] { type Out = Parrot }

    def select[T](w: WitnessWith[Select])(x: w.instance.Out): w.instance.Out = x

    println(select("cat")(Cat())) // Cat()
    //println(select("dog")(Cat())) // does not compile
    println(select("dog")(Dog())) // Dog()
    println(select("par")(Parrot())) // Parrot()
  }
}
