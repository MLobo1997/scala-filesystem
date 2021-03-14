package com.mlobo
package commands

import filesystem.State

import org.scalatest.TryValues._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MkdirTest extends AnyFunSuite {

  test("Mkdir with illegal chars") {
    val name = "test/test"
    assert(
      new Mkdir(name)(State.clean).output
        .equals(s"$name must not contain separators")
    )
  }

  test("Mkdir with 1 dir") {
    val name = "test"
    val newState = new Mkdir(name)(State.clean)
    newState.root.getEntry(name).success.value.name shouldBe name
    newState.wd.getEntry(name).success.value.name shouldBe name
  }
}
