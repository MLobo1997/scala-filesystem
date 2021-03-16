package com.mlobo
package commands

import files.File
import filesystem.State

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class TouchTest extends AnyFunSuite {
  test("Touch creates file") {
    val state = new Touch("test")(State.clean)
    state.root.contents.find(_.name == "test").get match {
      case _: File => Success
      case _       => throw new RuntimeException
    }
  }

}
