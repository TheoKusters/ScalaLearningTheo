import scala.annotation.tailrec
import scala.io.*
import java.security.MessageDigest

object Aoc2016Day05 extends App :

//  --- Part One ---

  def md5(s: String) =
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map("%02X".format(_))
      .mkString

  def findHash(base: String, suffix: Int, startZeroes: Int, fixed: Boolean = true): String =
    val zeroes = Array.fill[Byte](startZeroes)(0).mkString
    @tailrec
    def go(n: Int, a: Int, password: Array[String]): String =
      if !password.contains("_") then password.mkString("")
      else
        val hex = md5(base + n.toString)
        if hex.startsWith(zeroes) then
          if fixed then
            password.update(a, hex.slice(5,6))
            println(s"Password: ${password.mkString}")
          else
            val pos : Int = hex.slice(5,6).toList(0).toInt - '0'.toInt
            if (pos >= 0 & pos <= 7) then
              if (password(pos) == "_") then
                password.update(pos, hex.slice(6,7))
                println(s"Password: ${password.mkString}")
          go(n + 1, a + 1, password)
        else go(n + 1, a, password)
    go(suffix, 0, Array.fill[String](8)("_"))

  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init
  val doorId = "ojvtpuvg"
  val answer1 = findHash(doorId,0,5)
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis
  val answer2 = findHash(doorId,0,5, false)
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
