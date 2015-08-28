package com.example

import scala.io.{BufferedSource, Source}

object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

//    val urlSource: BufferedSource = Source.fromURL("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt")
//    val res: String = urlSource.mkString
    val resBuilder: StringBuilder = new StringBuilder("4 4\n")
    resBuilder.append("4 8 7 3\n");
    resBuilder.append("2 5 9 3\n");
    resBuilder.append("6 3 5 2\n");
    resBuilder.append("4 4 1 6\n");

    val res = resBuilder.toString()
    println(res)

    val perLine: Array[String] = res.split("\n")
    println(s"perLine.length: ${perLine.length}")

    println(s"perLine(0): ${perLine(0)}")
    println(s"perLine(1): ${perLine(1)}")
    println(s"perLine(1).length: ${perLine(1).split(" ").length}")

    val grids: Array[Array[Int]] = perLine.map((str) => str.split(" ").foldLeft(Array[Int]()) { (z, s) =>
      z :+ Integer.parseInt(s) })

    println(grids.toString)
  }
}
