import scala.annotation.tailrec
import scala.io.*

object Aoc2015Day11 extends App :

  // Todo: Check if end of string finishes well, test with Vector with 2 elements
  def nextPw(current: Vector[Int]): Vector[Int] =
    def loop (c: Vector[Int], n: Vector[Int]): Vector[Int] =
      c match
        case h +: t if (h <  'z'.toInt)  => n :+ (h+1) :++ t
        case h +: t if (h >= 'z'.toInt)  => loop (t, (n :+ 'a'.toInt) )
    loop (current, Vector.empty)

  def newPw(cur: Vector[Int]): Vector[Int] =
    val pw = nextPw(cur)
    if (checkPw(pw))
    then pw
    else newPw(pw)

  def twoPair(pw: Vector[Int]): Boolean =
    def pair (pw: Vector[Int], pairs: Int): Boolean =
      pw match
        case h1 +: h2 +: t if (h2 == h1 ) => pair(t, pairs + 1)
        case h1 +: t                      => pair(t, pairs)
        case _                            => pairs >= 2
    pair(pw, 0)

  def isSeq(pw: Vector[Int]): Boolean =
    def testSeq(pw: Vector[Int]): Boolean =
      pw match
        case h1 +: h2 +: h3 +: _ if (h3 == h2-1 && h2 == h1-1)  => true
        case h1 +: t                                            => testSeq(t)
        case _                                                  => false
    testSeq(pw)

  def validChars(pw: Vector[Int]): Boolean =
    val inValid : Vector[Int] = Vector('i'.toInt,'l'.toInt,'o'.toInt)
    def testValid(pw: Vector[Int]): Boolean =
      pw match
        case h +: _ if inValid.indexOf(h) >= 0  => false
        case h +: t                             => testValid(t)
        case _                                  => true
    testValid(pw)

  def checkPw(pw: Vector[Int]): Boolean =
    twoPair(pw) && validChars(pw) && isSeq(pw)

  // Todo : Could be used if a password contains an invalid character so the number of iterations is limited
  def sanitize(pw: String): String =
    def loop (in: Vector[Char], out: Vector[Char], toA: Boolean) : Vector[Char] =
      if toA then
        in match
          case h +: t   => loop(t, out :+ 'a', toA)
          case Vector() => out :+ 'a'
      else
        in match
          case h +: t if (h == 'i' && !toA) => loop(t, out :+ 'j', true)
          case h +: t if (h == 'l' && !toA) => loop(t, out :+ 'm', true)
          case h +: t if (h == 'o' && !toA) => loop(t, out :+ 'p', true)
          case h +: t                       => loop(t, out :+ h, toA)
          case h +: _                       => out :+ h
    loop(pw.toVector, Vector.empty, false).mkString("")

  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(3).init

  val alpha = "abcdefghijkmlnopqrstuvwxyz".toVector
  val num = alpha.map(a => a.toInt)
  val input1 = "hepxcrrq".reverse.toVector.map(letter => num( alpha.indexOf(letter) ))

  val answer1 = newPw(input1).map(number => alpha( num.indexOf(number))).mkString.reverse
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")
  assert(answer1 == "hepxxyzz")

  //  --- Part Two ---
  val start2 = System.currentTimeMillis

  val input2 = answer1.reverse.toVector.map(letter => num( alpha.indexOf(letter) ))
  val answer2 = newPw(input2).map(number => alpha( num.indexOf(number))).mkString.reverse
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
  assert(answer2 == "heqaabcc")
