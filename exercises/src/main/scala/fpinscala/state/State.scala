package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rnRaw, rngNext) = rng.nextInt
    val rn = if (rnRaw == Int.MinValue) Int.MaxValue else Math.abs(rnRaw)
    (rn, rngNext)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap[Int, Int](nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })

  def double(rng: RNG): (Double, RNG) = {
    val (rn, rngNext) = nonNegativeInt(rng)
    (rn / Int.MaxValue.toDouble, rngNext)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intRn, rngNext) = rng.nextInt
    val (doubleRn, rngOutput) = double(rngNext)
    ((intRn, doubleRn), rngOutput)
  }

  def intDoubleViaMap2: Rand[(Int, Double)] = map2(nonNegativeInt, double)((_, _))

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intRn, doubleRn), rngOutput) = intDouble(rng)
    ((doubleRn, intRn), rngOutput)
  }

  def doubleIntViaMap2: Rand[(Double, Int)] = map2(double, nonNegativeInt)((_, _))

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleRn1, rngNext1) = double(rng)
    val (doubleRn2, rngNext2) = double(rngNext1)
    val (doubleRn3, rngOutput) = double(rngNext2)
    ((doubleRn1, doubleRn2, doubleRn3), rngOutput)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = List.fill(count)(1).foldLeft[(List[Int], RNG)]((Nil, rng)) {
    case ((l, r), _) =>
      val (rn, rngNext) = r.nextInt
      (rn :: l, rngNext)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (rna, rngA) = ra(rng)
      val (rnb, rngB) = rb(rngA)
      (f(rna, rnb), rngB)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldRight[(List[A], RNG)]((Nil, rng))(
    (rand, state) => {
      val (list, rngCurrent) = state
      val (rn, rngNext) = rand(rngCurrent)
      (rn :: list, rngNext)
    }
  )

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(nonNegativeInt _))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (rand, rngNext) = f(rng)
    g(rand)(rngNext)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit[B](f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => {
    rng => {
      val (b, rngOut) = rb(rng)
      (f(a, b), rngOut)
    }
  })

}
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(
    s => {
     val (a,s1) = run(s)
      (f(a),s1)
    }
  )

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =  State(s=> {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a,b),s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
      s => {
        val  (a, s1) = run(s)
        f(a).run(s1)
      }
    )
}
object State {
  type Rand[A] = State[RNG, A]

  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = State(
    s0 => fs.foldRight[(List[A], S)]((Nil, s0))(
    (e, acc) => {
      val (list, s) = acc
      val (v, s1) = e.run(s)
      (v :: list, s1)
    }
  ))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => State((s: Machine) => (i, s) match {

    case (Coin, Machine(true, candies, coins)) if candies > 0 =>
      ((candies, coins + 1), Machine(locked = false, candies, coins + 1))

    case (Turn, Machine(false, candies, coins)) =>
      ((candies - 1, coins), Machine(locked = true, candies, coins + 1))

    case (_, m) =>
      ((m.candies, m.coins), m)
  })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(inputs.map(update)).flatMap(_ => State(s => ((s.candies, s.coins), s)))

  def simulateMachineV1(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(Candy.update))
      s <- State[Machine, Machine](s => (s, s))
    } yield (s.coins, s.candies)
}
