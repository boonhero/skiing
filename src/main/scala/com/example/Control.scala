package com.example

import scala.util.{Failure, Try}

/**
 * Created by asales on 27/8/2015.
 */
object MovementResult extends Enumeration {
  type MovementResult = Value
  val OK, NOT_OK, LIMIT = Value
}

class Control {
  private var pointer = (0, 0)
  private var currentNumber: Int = 0;
  private var input: Array[Array[Int]] = Array[Array[Int]]()
  private var marks: Vector[String] = Vector[String]()

  def getPointer() : (Int, Int) = pointer
  def setPointer(x: Int, y: Int): Unit = {
    pointer = (x, y)
    println(s"setPointer x: ${x}")
    currentNumber = input(pointer._1)(pointer._2)
    marks = marks :+ s"${pointer._1}-${pointer._2}"
  }
  def getCurrentNumber() = currentNumber
  def getMarks() = marks

  def goNorth() = {
    val x = pointer._1

    if (x == 0) {
      MovementResult.LIMIT
    } else {
      println(s"x: ${x}")
      check(x - 1, pointer._2) match {
        case Some(num) if (num < currentNumber) => {
          setPointer(x - 1, pointer._2)
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def goSouth() = {
    val x = pointer._1

    if (x == input.length - 1) {
      MovementResult.LIMIT
    } else {

      check(x + 1, pointer._2) match {
        case Some(num) if (num < currentNumber) => {
          setPointer(x + 1, pointer._2)
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def goWest() = {
    val y = pointer._2

    if (y == 0) {
      MovementResult.LIMIT
    } else {

      check(pointer._1, y - 1) match {
        case Some(num) if (num < currentNumber) => {
          setPointer(pointer._1, y - 1)
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def goEast() = {
    val y = pointer._2

    if (y == input(pointer._1).length - 1) {
      MovementResult.LIMIT
    } else {

      check(pointer._1, y + 1) match {
        case Some(num) if (num < currentNumber) => {
          setPointer(pointer._1, y + 1)
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }
    }
  }

  private def check(x: Int, y: Int): Option[Int] = {
    val num = input(x)(y)
    if (!marks.contains(s"${x}-${y}")) {
      Some(num)
    } else {
      None
    }
  }
}

object Control {
  var control: Control = new Control

  def apply(input: Array[Array[Int]]) = {
    control = new Control
    control.input = input
    (control.setPointer _).tupled(control.pointer)
    control
  }

  def apply(x: Int, y: Int, input: Array[Array[Int]]) = {
    control = new Control
    control.input = input
    (control.setPointer _).tupled((x, y))
    control
  }

}
