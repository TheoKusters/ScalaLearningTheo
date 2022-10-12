package chapter02

import annotation.*

object IsSorted:

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    @tailrec
    def loop(n: Int): Boolean =
      if n >= as.length - 1 then true
      else if !ordered(as(n), as(n + 1)) then false
      else loop(n + 1)
    loop(0)

object Currying:
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

object printIsSorted extends App:

    import IsSorted.*
    println(isSorted(Array(1,2,3,4,5), (a: Int, b: Int) => b >= a))
    println(isSorted(Array(-0), (a: Int, b: Int) => b >= a))
    println(!isSorted(Array(1,6,9,2,12), (a: Int, b: Int) => b >= a))
    println(isSorted(Array('A','B'), (a: Char, b: Char) => b >= a))
    println(isSorted(Array('a','B'), (a: Char, b: Char) => b >= a))
    println(isSorted(Array(true,false), (a: Boolean, b: Boolean) => b >= a))
