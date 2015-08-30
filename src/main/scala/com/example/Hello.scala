package com.example

import scala.collection.immutable.Queue
import scala.io.{BufferedSource, Source}

object Hello {
  val NORTH = 0
  val SOUTH = 1
  val WEST = 2
  val EAST = 3

  var longestLength: Int = -1
  var highestDrop: Int = -1


  def startMoving(direction: Int, control: Control) = direction match {
    case NORTH => control.goNorth()
    case SOUTH => control.goSouth()
    case WEST => control.goWest()
    case EAST => control.goEast()
  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    //SEMI-REAL TEST
//    val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//    resBuilder.append("4 8 7 3\n")
//    resBuilder.append("2 5 9 3\n")
//    resBuilder.append("6 3 5 2\n")
//    resBuilder.append("4 4 1 6\n")

    //TEST PASSED
//    val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//    resBuilder.append("4\n")
//    resBuilder.append("3\n")
//    resBuilder.append("2\n")
//    resBuilder.append("1\n")

    //TEST PASSED
//      val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//      resBuilder.append("1\n")
//      resBuilder.append("2\n")
//      resBuilder.append("3\n")
//      resBuilder.append("4\n")

//    //TEST PASSED
//    val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//    resBuilder.append("4 5\n")
//    resBuilder.append("3 6\n")
//    resBuilder.append("2 7\n")
//    resBuilder.append("1 8\n")

     //TEST PASSED
//      val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//      resBuilder.append("1 2 3 4\n")

    //TEST PASSED
//    val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//    resBuilder.append("4 3 2 1\n")

    //TEST PASSED
//    val resBuilder: StringBuilder = new StringBuilder("4 4\n")
//    resBuilder.append("5 6 7 8\n")
//    resBuilder.append("4 3 2 1\n")

    //TEST ON GOING
      val resBuilder: StringBuilder = new StringBuilder("4 4\n")
      resBuilder.append("3 5 2\n")

    val res = resBuilder.toString()
    println(res)

    var perLine: Array[String] = res.split("\n")
    perLine = perLine.slice(1, perLine.length)

    //convert to Array[Array[Int]]
    val grids: Array[Array[Int]] = perLine.map((str) => str.split(" ").foldLeft(Array[Int]()) { (z, s) =>
      z :+ Integer.parseInt(s) })

    Input.grid = grids

    //print grid
    grids.foreach(row => {
      row.foreach(col => print(s"${col} "))
      println()
    })

//    Input.grid(0)(2)

    //iterate
    grids.zipWithIndex.foreach(row => row._1.zipWithIndex.foreach(col => {
      println(s"row: ${row._2}")
      println(s"col: ${col._2}")

      val startQueue = collection.immutable.Queue[Control](Control(row._2, col._2))

      Iterator.iterate(startQueue) { controlQueueObject =>
        val (control, controlQueue) = controlQueueObject.dequeue

        println(s"controlQueue.size(): ${controlQueue.size}" )
        //countOks
        val routes = control.checkAllRoutes()
        val okRoutes = routes.filter(route =>  route._1 == MovementResult.OK)
        val okCount = okRoutes.count(route => route._1 == MovementResult.OK)
        println(s"okCount: ${okCount}")
        okCount match {
          case 0 => {
            val marks = control.getMarks()
            println(s"marks.size: ${marks.size}")
            marks.size match {
              case size if (size >= longestLength) => {
                longestLength = size
                val firstNumber: Int = findFromGrid(marks(0))
                val secondNumber: Int = findFromGrid(marks(marks.size - 1))

                println(s"(firstNumber - secondNumber): ${firstNumber}-${secondNumber}")
                //get drop
                (firstNumber - secondNumber) match {
                  case result if (result > highestDrop) => {
                    highestDrop = result

                    //keep queuing
                    controlQueue
                  }
                  case _ => {
                    //keep queuing
                    controlQueue
                  }
                }
              }
              case _ => {
                //keep queuing
                controlQueue
              }
            }
          }
          case 1 => {
            startMoving(okRoutes(0)._2, control)
            controlQueue.enqueue(control)
          }
          case _ => {
            println("accessing okCounts more than 2...")
            okRoutes.foreach(f => {
              val newControl = control.copy()
              startMoving(f._2, newControl)
              println(s"generated new control: ${newControl.toString()}")
              controlQueue.enqueue(newControl)
            })
          }

            //keep queuing
            controlQueue
        }
      }.takeWhile(!_.isEmpty).foreach(identity)


      println(s"highestDrop: ${highestDrop}")
      println(s"longestLength: ${longestLength}")
      println()
    }))

    println(s"final highestDrop: ${highestDrop}")
    println(s"final longestLength: ${longestLength}")
    println()
  }

  /**
   * @param str the string from control.marks
   */
  def findFromGrid(str: String): Int = {
    val split: Array[String] = str.split("-")
    val x = Integer.parseInt(split(0))
    val y = Integer.parseInt(split(1))
    Input.grid(x)(y)
  }


}
