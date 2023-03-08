import scala.annotation.tailrec
import scala.io.*

object Aoc2016Day07 extends App :

  case class IpAddress(ipAddress: String):
    def supportTLS: Boolean =
      val (superNet, hyperNet) = ipAddress.split("\\[|\\]").zipWithIndex.partition(_._2 % 2 == 0)
      superNet.exists(hasABBA) && !hyperNet.exists(hasABBA)

    def supportSSL: Boolean =
      val (superNet, hyperNet) = ipAddress.split("\\[|\\]").zipWithIndex.partition(_._2 % 2 == 0)
      superNet.flatMap(getABA).exists(aba => hyperNet.exists(hasBAB(_,aba)))

    private def hasABBA(addressPart: (String, Int)): Boolean =
      val partList = addressPart._1.sliding(4)
      partList.foldLeft(false)((b,v) => b | (v(0) == v(3) & v(1) == v(2)) & v(0) != v(1))

    def getABA(addressPart: (String, Int)): Vector[String] =
      val partList = addressPart._1.sliding(3)
      partList.filter(v => v(0) == v(2) & v(0) != v(1)).toVector

    def hasBAB(addressPart: (String, Int), aba: String): Boolean =
      addressPart._1.sliding(3).exists(bab => bab(0) == aba(1) & bab(2) == aba(1) & bab(1) == aba(0))

//  --- Part One ---
  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

  val input =
    Source
      .fromResource("inputAoc2016Day07.txt")
      .getLines
      .toList

  val answer1 = input.map(IpAddress.apply).count(_.supportTLS)
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis

  val answer2 = input.map(IpAddress.apply).count(_.supportSSL)
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
