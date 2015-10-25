import shapeless._
import shapeless.ops.tuple.IsComposite
import shapeless.poly._
import shapeless.ops.hlist._

object Day2 extends App {
  {
    class BiMapIS[K, V]
    implicit val intToString = new BiMapIS[Int, String]
    implicit val doubleToString = new BiMapIS[Double, String]
    val hm = HMap[BiMapIS](23 -> "i")
    val hm2 = hm + (5.23 -> "d")
    val hm3 = hm2 + (5.23 -> "dd")

    println(hm.get(23)) // Some(i)
    println(hm3.get(5.23)) // Some(dd)
  }

  {
    class BiMapIS[K, V]
    implicit val intToString = new BiMapIS[Int, String]
    implicit val StringToInt = new BiMapIS[String, Int]
    val hmap = HMap[BiMapIS](
      1 -> "one", 2 -> "two", 3 -> "three",
      "one" -> 1, "two" -> 2, "three" -> 3)

    import hmap._
    val xs: Int :: String :: Int :: HNil = 1 :: "two" :: 3 :: HNil
    val xsm: String :: Int :: String :: HNil = xs map hmap
    println(xsm) // one :: 2 :: three :: HNil
  }
}
