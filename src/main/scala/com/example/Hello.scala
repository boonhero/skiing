package com.example

import scala.io.{BufferedSource, Source}

object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val urlSource: BufferedSource = Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt")
    val res: String = urlSource.mkString

    println(res)

    val perLine: Array[String] = res.split("\n")
    println(s"perLine.length: ${perLine.length}")

    println(s"perLine(0): ${perLine(0)}")
    println(s"perLine(1): ${perLine(1)}")
    println(s"perLine(1).length: ${perLine(1).split(" ").length}")

  }
}
