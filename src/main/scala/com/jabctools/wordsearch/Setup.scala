package com.jabctools.wordsearch

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Setup {
  @tailrec
  def askForSize(intro: Boolean = true): Int = {
    if (intro) {
      println(
        s"""----------------------------------------------------
           |Type how large you want the word search to be below.
           |${Console.BLUE}Recommended: 20${Console.RESET}
           |----------------------------------------------------""".stripMargin
      )
    }

    print("Size of word search: ")
    Try(io.StdIn.readInt).toOption match {
      case Some(v) if v > 5 && v < 30 => v
      case _ =>
        println("You must enter a valid number greater than 5 and less than 30")
        askForSize(false)
    }
  }

  @tailrec
  def askForWordCount(size: Int, intro: Boolean = true): Int = {
    if (intro) {
      val recommendedMin = Math.ceil((size * size) / 15).toInt
      val recommendedMax = Math.ceil((size * size) / 5).toInt
      println(
        s"""----------------------------------------------------
           |Enter the number of words you want in the search below.
           |${Console.BLUE}Note: Certain words may be ignored if there is not enough space to fit them.${Console.RESET}
           |${Console.BLUE}Recommended words for size chosen: $recommendedMin - $recommendedMax ${Console.RESET}
           |----------------------------------------------------""".stripMargin
      )
    }

    print("Number of words: ")
    Try(io.StdIn.readInt).toOption match {
      case Some(v) if v > 0 => v
      case _ =>
        println("You must enter a valid number greater than 0")
        askForWordCount(size, intro = false)
    }
  }

  @tailrec
  def askForWords(
      count: Int,
      size: Int,
      intro: Boolean = true,
      words: List[String] = List.empty
  ): List[String] = {
    if (intro) {
      println(
        s"""----------------------------------------------------
           |Start typing the words that you want to be in the search below.
           |${Console.BLUE}Note: Words should be longer than 3 characters.${Console.RESET}
           |----------------------------------------------------""".stripMargin
      )
    }

    val current = words.length + 1
    val input = io.StdIn.readLine(s"Word #$current/$count: ")
    val errors = validateInput(input, size, words)
    val newList = input.toUpperCase :: words

    if (newList.length < count && errors.isEmpty) {
      askForWords(count, size, intro = false, newList)
    } else if (errors.nonEmpty) {
      println(errors.map(_.prependedAll(" - ")).mkString("\n"))
      askForWords(count, size, intro = false, words)
    } else {
      newList.reverse
    }
  }

  private def validateInput(
      input: String,
      size: Int,
      words: List[String]
  ): List[String] = {
    val errors = ArrayBuffer.empty[String]

    if (input.length < 3 || input.length > size)
      errors += s"Word must be longer than 3 characters and less than the word search size ($size letters)"

    if (!input.forall(_.isLetter))
      errors += "Words can only contain letters"

    if (words.contains(input.toUpperCase))
      errors += "Word already exists"

    words.find(_.contains(input.toUpperCase)) match {
      case Some(word) =>
        errors += s"You cannot enter a subset of another word previously entered ($word)"
      case None => ()
    }

    val inputContainsSubset = words.foldLeft(List.empty[String]) { (acc, word) =>
      if (input.toUpperCase.contains(word)) word :: acc else acc
    }

    if (inputContainsSubset.nonEmpty)
      errors += s"You have previously entered word(s) " +
        s"that are subsets of your input: ${inputContainsSubset.mkString(", ")}"

    errors.toList
  }
}
