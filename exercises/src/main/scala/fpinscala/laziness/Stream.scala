package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =  this match {
      case Empty => Nil
      case Cons(h,t)=> h() :: t().toList
    }

  def toListViaRF: List[A] = this.foldRight(Nil:List[A])(_ :: _)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(),t().take(n-1))
    case Cons(h, _) if n == 0 => cons(h(), empty)
    case _ => empty
  }

 

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty // exception if n>0?
    case Cons(_,t)  => if (n==0){ this } else {
      t().drop(n-1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match{
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((h, t) => if (p(h)){cons(h,t)} else empty)


  def headOption: Option[A] = this.foldRight(None: Option[A])((h,_)=>Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    this.foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: Stream[B]): Stream[B] =
    this.foldRight(s)(cons(_, _))

  def flatMap[B](p: A => Stream[B]): Stream[B] =
    this.foldRight(empty[B])((h, t) => p(h).append(t))
  
  def mapViaUnfold[B](f: A=>B): Stream[B] = unfold[B, Stream[A]](this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold[A,(Int,Stream[A])] ((n, this)){
      s => s._2 match {
        case Cons(h,t) if s._1 != 0 => Some(h(), (s._1-1, t()))
        case _ => None
      }
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold[A,Stream[A]] (this){
      case Cons(h,t) if f(h())=> Some(h(), t())
      case _ => None
  }

  def zipWith[B, C](s: Stream[B], f: (A, B)=> C): Stream[C] = unfold[C, (Stream[A], Stream[B])]((this, s)){
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] 
  = unfold[(Option[A], Option[B]), (Stream[A], Stream[B])] ((this, s2)) {
    case (Empty, Empty) => None
    case _ @p =>
      val s1 = p._1 match {
        case Cons(h, t) => (Some(h()), t())
        case _ => (None, p._1)
      }
      val s2 = p._2 match {
        case Cons(h, t) => (Some(h()), t())
        case _ => (None, p._2)
      }
      Some((s1._1, s2._1), (s1._2, s2._2))
  }
  
  def startsWith[B](s: Stream[B]): Boolean = this.zipAll[B](s).foldRight(true)(
    (e, state) => e match {
      case (Some(e1), Some(e2)) if e1==e2 => state
      case (Some(_), None) => true
      case _ => false
    }
  )
  
  def tails: Stream[Stream[A]] = Stream(this).append(unfold[Stream[A], Stream[A]](this){
    case Cons(_,t) => Some((t(), t()))
    case _ => None
  })
  
  def hasSubsequence[B>:A](s: Stream[B]): Boolean = tails exists( _ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = 
    this match {
      case Cons(h,t) =>
        val tail = t().scanRight(z)(f)
        cons[B](f(h(), tail.headOption.get), tail)
      case _ => Stream(z)
    }

  def scanRightViaFR[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z))((a,s)=>{
    lazy val ps:B = s.headOption.get
    cons(f(a,ps),s)
  })
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a:A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fib(): Stream[Int] = {
    def fib(a: Int, b: Int): Stream[Int] = Stream.cons(a, fib(b, a+b))
    fib(0,1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def fibViaUnfold(): Stream[Int] = 
    unfold[Int,(Int,Int)]((0,1)){ case (n1, n2) => Some((n1, (n2, n1+n2)))}
  
  def onesViaUnfold(): Stream[Int] = unfold[Int,Int](1)(_ => Some((1, 1)))
  def constantViaUnfold[A](a:A): Stream[A] = unfold[A,A](a)(_ => Some((a, a)))
  def fromViaUnfold(n: Int): Stream[Int] = unfold[Int,Int](n)(s => Some((s, s+1)))

}
