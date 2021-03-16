package com.mlobo
package commands

import filesystem.State
import utils.Path

import org.scalatest.TryValues._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MkdirTest extends AnyFunSuite {

  test("Mkdir with 1 dir") {
    val name = "test"
    val newState = new Mkdir(Path(name))(State.clean)
    newState.root.getEntry(name).success.value.name shouldBe name
    newState.wd.getEntry(name).success.value.name shouldBe name
  }
}
