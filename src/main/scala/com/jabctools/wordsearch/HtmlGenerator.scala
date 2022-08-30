package com.jabctools.wordsearch

import java.io.{File, PrintWriter}
import java.util.Date

object HtmlGenerator {
  private val counterPath = "src/main/resources/counter.txt"

  private def createCounterFile(): Unit = {
    val writer = new PrintWriter(new File(counterPath))
    writer.print("0")
    writer.close()
  }

  private def getCounter: Int = {
    if (new File(counterPath).exists()) {
      val counter = io.Source.fromResource("counter.txt")
      val line = counter.getLines.toList.head
      counter.close()
      line.toInt
    } else {
      createCounterFile()
      0
    }
  }

  private def incrementCounter(currentCount: Int): Unit = {
    val writer = new PrintWriter(new File(counterPath))
    writer.print(s"${currentCount + 1}")
    writer.close()
  }


  private def htmlHead(id: Int): String = {
    s"""
       |<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |<meta charset="UTF-8">
       |<title>Word Search #$id</title>
       |<style>
       |body {
       |    font-family: sans-serif;
       |    font-size: 1rem;
       |    max-width: 800px;
       |    margin: auto;
       |}
       |
       |h1 {
       |    text-align: center;
       |    margin-bottom: 0;
       |}
       |
       |.createdAt {
       |    display: block;
       |    font-size: 0.75rem;
       |    text-align: center;
       |    font-style: italic;
       |    margin-top: 0;
       |    margin-bottom: 0.25rem;
       |    color: #a0a0a0;
       |}
       |
       |.grid {
       |    display: flex;
       |    flex-direction: column;
       |    width: 500px;
       |    height: 500px;
       |    margin: auto;
       |    border: 2px solid #c0c0c0;
       |}
       |
       |.row {
       |    flex: 1;
       |    display: flex;
       |    flex-direction: row;
       |    flex-wrap: wrap;
       |    justify-content: center;
       |    align-items: center;
       |}
       |
       |.cell {
       |    flex: 1;
       |    display: flex;
       |    font-size: 1.5rem;
       |    font-weight: bold;
       |    align-items: center;
       |    justify-content: center;
       |}
       |
       |.cell span {
       |    display: block;
       |}
       |
       |.words {
       |    margin-top: 2rem;
       |}
       |
       |.wordlist {
       |    column-count: 5;
       |}
       |
       |.wordlist li {
       |    padding: 0.25rem 0;
       |}
       |</style>
       |</head>
       |""".stripMargin
  }

  private def htmlContent(
    id: Int,
    timestamp: String,
    words: List[String],
    matrix: Array[Array[Char]]
  ): String = {
    val rowsHtml = matrix.map { row =>
      val cells = row
        .map(c =>
          s"""<div class="cell"><svg viewBox="0 0 28 28"><text x="50%" y="22" text-anchor="middle">$c</text></svg></div>"""
        )
        .mkString("\n")
      s"""<div class="row">\n$cells\n</div>\n"""
    }.mkString

    val wordsHtml = words.map(word => s"""<li>$word</li>""").mkString("\n")

    s"""
       |<body>
       |<h1>Word Search #$id</h1>
       |<span class="createdAt">$timestamp</span>
       |<div class="grid">
       |$rowsHtml
       |</div>
       |<div class="words">
       |<ol class="wordlist">
       |$wordsHtml
       |</ol>
       |</div>
       |</body>
       |</html>
       |""".stripMargin
  }

  def writeHTMLFiles(
    words: List[String],
    matrix: Array[Array[Char]],
    matrixAnswers: Array[Array[Char]]
  ): Unit = {
    val counter = getCounter
    val timestamp = new Date().toString

    val wordSearchWriter = new PrintWriter(new File(s"html/wordsearch-$counter.html"))
    wordSearchWriter.print(htmlHead(counter))
    wordSearchWriter.print(htmlContent(counter, timestamp, words, matrix))
    wordSearchWriter.close()

    val answerWriter = new PrintWriter(new File(s"html/wordsearch-$counter-answers.html"))
    answerWriter.print(htmlHead(counter))
    answerWriter.print(htmlContent(counter, timestamp, words, matrixAnswers))
    answerWriter.close()

    incrementCounter(counter)
  }
}
