package funprog.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  lazy val int2: Rand[Int] = ???

  def unit[A](a: A): Rand[A] = ???

  def map[S,A,B](a: S => (A,S))(f: A => B): S => (B, S)= ???

  // a Rand[C] is RNG => (C, RNG)
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def map2WithFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  // we want to return a function that takes a RNG and returns a RNG => (B, RNG)
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def mapWithFM[A, B](s: Rand[A])(f: A => B): Rand[B] = ???

  def nonNegativeLessThan(n: Int): Rand[Int] = ???

  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  def double(rng: RNG): (Double, RNG) = ???

  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  def ints(count: Int)(rng: RNG): ((List[Int]), RNG) = ???

  def nonNegativeEven: Rand[Int] = ???

  def double2(rng: RNG): (Double, RNG) = ???

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def ints2(count: Int): Rand[List[Int]] = ???
}
