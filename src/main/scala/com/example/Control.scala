package com.example

import scala.util.{Failure, Try}

/**
 * Created by asales on 27/8/2015.
 */
object MovementResult extends Enumeration {
  type MovementResult = Value
  val OK, NOT_OK, LIMIT = Value
}

object Input {
  var grid: Array[Array[Int]] = Array[Array[Int]]()
}

class Control {
  private var pointer = (0, 0)
  private var currentNumber: Int = 0;
  private var marks: Vector[String] = Vector[String]()

  def copy() : Control = {
    val control = new Control()
    control.setPointer(pointer._1, pointer._2)
    control.setCurrentNumber(this.currentNumber)
    control.setMarks(this.marks)
    control
  }

  def getPointer() : (Int, Int) = pointer
  def setPointer(x: Int, y: Int): Unit = {
    pointer = (x, y)
    currentNumber = Input.grid(pointer._1)(pointer._2)
    marks = marks :+ s"${pointer._1}-${pointer._2}"
    println(s"setPointer x: ${x} y: ${y} val: ${getCurrentNumber()}")
  }
  def getCurrentNumber() = currentNumber
  def setCurrentNumber(currentNumber: Int): Unit = {
    this.currentNumber = currentNumber
  }
  def getMarks() = marks
  def setMarks(marks: Vector[String]): Unit = {
    this.marks = marks
  }

  def checkNorth() = {
    val x = pointer._1

    if (x == 0) {
      MovementResult.LIMIT
    } else {
      println(s"x: ${x}")
      check(x - 1, pointer._2) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def checkAllRoutes() = {
    List(checkNorth(), checkSouth(), checkWest(), checkEast()).zipWithIndex
  }

  def checkSouth() = {
    val x = pointer._1

    if (x == Input.grid.length - 1) {
      MovementResult.LIMIT
    } else {

      check(x + 1, pointer._2) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def checkWest() = {
    val y = pointer._2

    if (y == 0) {
      MovementResult.LIMIT
    } else {

      check(pointer._1, y - 1) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def checkEast() = {
    val y = pointer._2

    if (y == Input.grid(pointer._1).length - 1) {
      MovementResult.LIMIT
    } else {
      check(pointer._1, y + 1) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }
    }
  }

  def goNorth() = setPointer(pointer._1 - 1, pointer._2)
  def goSouth() = setPointer(pointer._1 + 1, pointer._2)
  def goWest() = setPointer(pointer._1, pointer._2 - 1)
  def goEast() = setPointer(pointer._1, pointer._2 + 1)

  private def check(x: Int, y: Int): Option[Int] = {
    println(s"check x: ${x} y:${y}")
    val num = Input.grid(x)(y)
    if (!marks.contains(s"${x}-${y}")) {
      Some(num)
    } else {
      None
    }
  }
}

object Control {
  var control: Control = Control()

  def apply() = {
    control = new Control()
    (control.setPointer _).tupled(control.pointer)
    control
  }

  def apply(x: Int, y: Int) = {
    control = new Control()
    (control.setPointer _).tupled((x, y))
    control
  }

}
