package com.jabctools.wordsearch

import scala.annotation.tailrec

object WordSearchGenerator {

  @tailrec
  private def promptGeneration(words: List[String], searchSize: Int): Unit = {
    val wordMatrix = WordMatrix(words, searchSize)

    print(
      s"${Console.BLUE}Do you want to try generating again? (yes/no): ${Console.RESET}"
    )

    val tryAgain = io.StdIn.readBoolean()

    if (tryAgain) {
      promptGeneration(words, searchSize)
    } else {
      print(s"${Console.BLUE}What do you want to name the search? (optional): ${Console.RESET}")
      val searchName = io.StdIn.readLine()
      HtmlGenerator.writeHTMLFiles(
        wordMatrix.getPlacedWords,
        wordMatrix.getMatrix,
        wordMatrix.getMatrixAnswers,
        searchName
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val searchSize = Setup.askForSize()
    val wordCount = Setup.askForWordCount(searchSize)
    val words = Setup.askForWords(wordCount, searchSize)

    println(s"Word search size: $searchSize x $searchSize")
    println(s"Word count: $wordCount")
    println(s"Words list: ${words.mkString(", ")}")

    promptGeneration(words, searchSize)
  }
}
