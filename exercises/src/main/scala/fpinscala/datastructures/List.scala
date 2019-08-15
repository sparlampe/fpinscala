package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, tail) => tail
      case _ => throw new IllegalArgumentException("Empty list has no tail")

    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    Cons(h, tail(l))
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0){
        l
      } else {
        l match {
          case Nil => Nil // throw new IllegalArgumentException(s"The list is shorter than the number of elements to drop.")
          case Cons(_, tail) => drop(tail, n-1)
        }
      }
   }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, tail) if f(h) =>  dropWhile(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException("Cant substract the last element of an empty list.")
      case Cons(_,Nil) => Nil
      case Cons(a, tail) => Cons(a, init(tail))
    }
  }

  def lengthw[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(_, tail) => 1 + lengthw(tail)
    }
  }

  def length[A](l: List[A]): Int = {
   foldRight(l,0)((_,acc)=> acc + 1)
  }
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, tail) => foldLeft(tail, f(z,h))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def sumFL(l: List[Int])=   foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double])= foldLeft(l, 1.0)(_ * _)

  def foldLeftOverRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(l),z)((a,b)=>f(b,a))
  }

  def foldLRightOverLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l),z)((b,a)=>f(a,b))
  }

  def appendFR[A](l: List[A], r: List[A]): List[A] =
    foldRight(l,r)(Cons(_, _))

  def appendFL[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1),a2)((l2, e1)=> Cons(e1, l2))


  def add1(l:List[Int]):List[Int]={
    foldRight(l, Nil:  List[Int])((h, t)=> Cons(h+1,t))
  }

  def toStr(l:List[Double]):List[String]={
    foldRight(l, Nil: List[String])((e, acc)=> Cons(e.toString,acc))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((e, acc)=> Cons(f(e),acc))

  def filter[A](l: List[A], f: A=>Boolean):List[A]={
    foldRight[A, List[A]](l, Nil)((e, acc)=> if (f(e)) Cons(e, acc) else acc)
  }

  def flatMap[A,B](as:List[A], f:A=>List[B]):List[B]={
    foldRight[A, List[B]](as, Nil)((e, acc)=> appendFR(f(e), acc))
  }

  def filterFlatMap[A](as:List[A], f:A=>Boolean):List[A]={
    flatMap[A,A](as, a=> if(f(a)) List(a)else Nil)
  }

  def addList(l1:List[Int], l2:List[Int]):List[Int]={
     if(l1==Nil || l2== Nil){
       Nil
     } else {
       val Cons(head1, tail1) = l1
       val Cons(head2, tail2) = l2
       Cons(head1+head2, addList(tail1, tail2))
     }
  }

  def zipWith[A,B,C](l1:List[A], l2:List[B], f: (A,B)=>C):List[C]={
    if(l1==Nil || l2== Nil){
      Nil
    } else {
      val Cons(head1, tail1) = l1
      val Cons(head2, tail2) = l2
      Cons(f(head1,head2), zipWith(tail1, tail2, f))
    }
  }


  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup,sub) match {
      case (_, Nil) => true
      case (Cons(supH, supT), Cons(subH,subT)) if supH==subH => startsWith(supT, subT)
      case _ => false
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => sub == Nil
        case _ if   startsWith(sup, sub) => true
        case Cons(_, tail)=> hasSubsequence(tail, sub)
    }
  }
