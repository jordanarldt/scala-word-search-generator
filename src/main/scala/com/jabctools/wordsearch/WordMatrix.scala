package com.jabctools.wordsearch

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object WordMatrix {
  private trait Direction
  private case object UpLeft extends Direction
  private case object Up extends Direction
  private case object UpRight extends Direction
  private case object Right extends Direction
  private case object DownRight extends Direction
  private case object Down extends Direction
  private case object DownLeft extends Direction
  private case object Left extends Direction

  private case class XY(x: Int, y: Int)
  private case class CharCoords(xy: XY, letter: Char)
  private case class FailedAttempt(xy: XY, direction: Direction)
}

case class WordMatrix(words: List[String], size: Int) {
  import WordMatrix._

  private val letters = 'A' to 'Z'
  private val matrix = Array.ofDim[Char](size, size)
  private val maxStartCombinations = size * size * 8
  private val placedLetters = ArrayBuffer.empty[CharCoords]
  private val placedWords = ArrayBuffer.empty[String]

  for {
    y <- matrix.indices
    x <- matrix(y).indices
  } matrix(y)(x) = '_'

  // Add all the words to the matrix, add the largest first
  words.sortBy(_.length).reverse.foreach(addWord(_))

  private def update(xy: XY, c: Char): Unit = matrix(xy.y)(xy.x) = c

  private def randomDirection: Direction = {
    val directions = List(UpLeft, Up, UpRight, Right, DownRight, Down, DownLeft, Left)
    directions(Random.nextInt(directions.size))
  }

  private def randomXY: XY = {
    val x = Random.nextInt(size)
    val y = Random.nextInt(size)
    XY(x, y)
  }

  private def allPointsValid(points: List[CharCoords]): Boolean = {
    val inBounds = points.forall(p =>
      p.xy.x >= 0 && p.xy.x <= size - 1 && p.xy.y >= 0 && p.xy.y <= size - 1
    )

    val conflicts = points.foldLeft(false) { (acc, p) =>
      placedLetters.find(_.xy == p.xy) match {
        case Some(c) if c.letter != p.letter => true
        case _                               => acc
      }
    }

    !conflicts && inBounds
  }

  // Instead of generating coordinates and direction randomly,
  // Use a blacklist of previously attempted placements that failed to make sure
  // each attempt has not been made previously.
  @tailrec
  private def randomAndUnusedXYDir(
      attempted: List[FailedAttempt]
  ): Option[(XY, Direction)] = {
    val xy = randomXY
    val direction = randomDirection
    val hasBeenTried = attempted.exists(a => a.xy == xy && a.direction == direction)

    if (hasBeenTried && attempted.length < maxStartCombinations) {
      randomAndUnusedXYDir(attempted)
    } else if (attempted.length >= maxStartCombinations) {
      None
    } else {
      Some((xy, direction))
    }
  }

  @tailrec
  private def addWord(
      word: String,
      attempted: List[FailedAttempt] = List.empty
  ): Unit = {
    randomAndUnusedXYDir(attempted) match {
      case Some((start, direction)) if word.length <= size =>
        val points = {
          direction match {
            case Up =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x, start.y - i), word(i))
            case UpRight =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x + i, start.y - i), word(i))
            case Right =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x + i, start.y), word(i))
            case DownRight =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x + i, start.y + i), word(i))
            case Down =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x, start.y + i), word(i))
            case DownLeft =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x - i, start.y + i), word(i))
            case Left =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x - i, start.y), word(i))
            case UpLeft =>
              for (i <- word.indices)
                yield CharCoords(XY(start.x - i, start.y - i), word(i))
          }
        }.toList

        if (allPointsValid(points)) {
          points.foreach { p =>
            placedLetters += p
            update(p.xy, p.letter)
          }
          placedWords += word
        } else {
          addWord(word, FailedAttempt(start, direction) :: attempted)
        }
      case None =>
        println(s"Ignoring word $word (Tried all $maxStartCombinations combinations)")
      case _ =>
        println(s"Ignoring word $word (Word is too large, exceeds bounds of matrix)")
    }
  }

  def printMatrixAnswers(): Unit = println(matrix.map(_.mkString(" ")).mkString("\n"))
  def printMatrix(): Unit = println(getMatrix.map(_.mkString(" ")).mkString("\n"))

  def getMatrix: Array[Array[Char]] = {
    val obfuscated = matrix.map(_.clone)

    for {
      y <- obfuscated.indices
      x <- obfuscated(y).indices
    } if (obfuscated(y)(x) == '_') {
      obfuscated(y)(x) = letters(Random.nextInt(letters.size))
    }

    obfuscated
  }

  def getMatrixAnswers: Array[Array[Char]] = matrix

  def getPlacedWords: List[String] = placedWords.sorted.toList
}
