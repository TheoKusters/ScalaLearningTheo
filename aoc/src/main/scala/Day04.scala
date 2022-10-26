//--- Day 4: The Ideal Stocking Stuffer ---
// Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically
// forward-thinking little girls and boys.
//
// To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes.
// The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal.
// To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
//
// For example:
//
// If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...),
// and it is the lowest such number to do so.
// If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970;
// that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
//
// Your puzzle answer was 254575.

import scala.annotation.tailrec
import scala.io.*
import java.security.MessageDigest

object Day04 extends App {

  val INPUT = "bgvyzdsv"
  val start1 = System.currentTimeMillis

  def md5(s: String) = {
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map("%02X".format(_))
      .mkString
  }

  def findHash (base: String, startZeroes: Int): Int =
    val zeroes = Array.fill[Byte](startZeroes)(0).mkString
    @tailrec
    def go(n: Int): Int =
      if (md5(base + n).startsWith(zeroes)) n
      else go(n + 1)
    go(0)

  println(s"Answer day 4, part 1: ${findHash(INPUT, 5)} [${System.currentTimeMillis - start1 }ms]")

//    --- Part Two ---
//  Now find one that starts with six zeroes.
//
//  Your puzzle answer was 1038736.

  val start2 = System.currentTimeMillis

  println(s"Answer day 4, part 2: ${findHash(INPUT, 6)} [${System.currentTimeMillis - start2 }ms]")
}
