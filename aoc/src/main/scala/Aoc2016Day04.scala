import scala.annotation.tailrec
import scala.io.*

object Aoc2016Day04 extends App :

  case class Room(name: String, sectorId: Int, checkSum: String):

    def isReal: Boolean =
      val encrypted = name.toList.filter(c => c != '-')
                          .groupBy(identity)
                          .view.mapValues(_.size)
                          .toList.groupBy(_._2)
                          .toSeq.sortBy(_._1)
                          .reverse
                          .map((_,y) => y.sortBy(_._1))
                          .flatten
                          .unzip
                          .head
                          .mkString("")
      encrypted.startsWith(checkSum)

    def decryptedRoomName: String =
      name.toList.foldLeft("")((d,c) => d + decryptLetter(c))

    private def decryptLetter(c: Char): Char =
      if (c == '-') then ' '
      else
        val shiftedLetter = c.toInt + (sectorId % 26)
        if (shiftedLetter > 122) then (shiftedLetter - 26).toChar
        else (shiftedLetter).toChar

  object Room:

    def fromString(s: String): Room =
      val r = s.trim.split("-(?!.*-.*)|\\[(?)|\\]")
      Room(r(0), r(1).toInt, r(2))

//  --- Part One ---
  val start1 = System.currentTimeMillis
  val day: String = this.getClass.getName.drop(10).init

  val roomVector =
    Source
      .fromResource("inputAoc2016Day04.txt")
      .getLines
      .map(Room.fromString).toVector

  val answer1 = roomVector.filter(room => room.isReal).foldLeft(0)((sumSectorIds, room) => sumSectorIds + room.sectorId)
  println(s"Answer day ${day}, part 1: ${answer1} [${System.currentTimeMillis - start1 }ms]")

  //  --- Part Two ---

  val start2 = System.currentTimeMillis

  val answer2 = roomVector.filter(room => room.isReal).filter(room => room.decryptedRoomName.startsWith("north")).head.sectorId
  println(s"Answer day ${day}, part 2: $answer2 [${System.currentTimeMillis - start2 }ms]")
