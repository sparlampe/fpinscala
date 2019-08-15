package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def sum[A](t: Tree[A]): Int = {
    t match {
       case Leaf(_) => 1
       case Branch(l, r) => 1 + sum(l) + sum(r)
     }
  }


  def sumTR[A](t: Tree[A]): Int = {
    @tailrec
    def sumAcc(l: scala.collection.immutable.List[Tree[A]], acc: Int): Int = {
      l match {
        case Leaf(_)::tail => sumAcc(tail, acc+1)
        case Branch(l,r)::tail => sumAcc(r::l::tail, acc+1)
        case _ => acc
      }
    }
    sumAcc(scala.collection.immutable.List(t), 0)
  }

  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => max(l) max max(r)
    }
  }

  def maxTr(t: Tree[Int]): Int = {
    @tailrec
    def maxAcc(l: scala.collection.immutable.List[Tree[Int]], acc: Int): Int = {
      l match {
        case Leaf(v)::tail => maxAcc(tail, acc max v)
        case Branch(l,r)::tail => maxAcc(r::l::tail, acc)
        case _ => acc
      }
    }
    maxAcc(scala.collection.immutable.List(t), scala.Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](t: Tree[A])(f: A=> B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A], init: B, f: (Tree[A], B) => B): B = {
    @tailrec
    def foldAcc(list: scala.collection.immutable.List[Tree[A]], init: B, f: (Tree[A], B) => B): B = {
      list match {
        case Leaf(_)::tail => foldAcc(tail, f(list.head,init), f)
        case Branch(l,r)::tail => foldAcc(r::l::tail, f(list.head,init), f)
        case _ => init
      }
    }
    foldAcc(scala.collection.immutable.List[Tree[A]](t), init, f)
  }

  def maxFold(t: Tree[Int]):Int = fold(t, Int.MinValue, (t: Tree[Int], acc: Int)=> t match { case Leaf(v) => v max acc; case _ => acc})
  def sumFold(t: Tree[Int]):Int = fold(t, Int.MinValue, (t: Tree[Int], acc: Int)=> t match { case Leaf(v) => v + acc; case _ => acc})
  def sizeFold[A](t: Tree[A]):Int = fold[A,Int](t, Int.MinValue, (_, acc: Int)=> acc+1)

  def foldB[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(foldB(l)(f)(g), foldB(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = foldB(t)(_=>1)(1+_ + _)

  def maximumViaFold(t: Tree[Int]): Int = foldB[Int,Int](t)(a=>a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = foldB(t)(_=>1)(1 + _ max _)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = foldB(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

}
