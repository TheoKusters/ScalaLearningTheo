package chapter03

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List:
  def sum(ints: List[Int]): Int =
    ints match
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)

  def product(ds: List[Double]): Double =
    ds match
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))

  // Exercise 3.2
  def tail[A](myList: List[A]): List[A] =
    myList match
      case Nil => Nil
      case Cons(_, xs) => xs

  // Exercise 3.3
  def setHead[A](newHead: A, myList: List[A]): List[A] =
    (newHead, myList) match
      case (_, Nil) => Cons(newHead, Nil)
      case (Nil, _) => Nil
      case (_, _) => Cons(newHead, tail(myList))

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    @tailrec
    def go[A](l: List[A], n: Int): List[A] =
      if (n < 1) l
      else if (l == Nil) l
        else go(tail(l), n - 1)
    go(l, n)

  def append[A](myList: List[A], element: A): List[A] =
    myList match
      case Nil => Cons(element, Nil)
      case Cons(head, Nil) => Cons(head, Cons(element, Nil))
      case Cons(head,tail) => Cons(head, append(tail, element))

  // Exercise 3.5
  def dropWhile[A](myList: List[A], f: A => Boolean): List[A] =
    myList match
      case Cons(head, tail) if f(head) => dropWhile(tail,f)
      case _ => myList

// Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

object Play extends App:
  import List.*

  //  Exercise 3.1
  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  println(x.toString)
  println(apply("Hi", " there"," you"," all."))
  println(tail(apply(1,2,3)))
  println(setHead(Nil, apply(1,2,3)))
  println(setHead(12, Nil))
  println(setHead(12, apply(1,2,3)))
  println(drop(apply(1,2,3),2))
  println(dropWhile(apply(1,2,3),x => x < 3))
  println(dropWhile(List(1,2,3),x => x < 3))
  println(init(apply(1,2,3)))
  println(append(apply(1,2,3),4))