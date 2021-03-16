package com.mlobo
package commands

import com.mlobo.filesystem.State
import org.scalatest.funsuite.AnyFunSuite

class CommandsTest extends AnyFunSuite {
  test("Mkdir, cd and ls") {
    val s1 = State.clean
    assert(new Ls()(s1).output.isEmpty)
    val s2 = new Mkdir("test")(s1)
    assert(new Ls()(s2).output.contains("test"))
    val s3 = new Cd("test")(s2)
    assert(new Ls()(s3).output.isEmpty)
    val s4 = new Touch("newFile")(new Mkdir("newDir")(s3))
    val output = new Ls()(s4).output
    assert(
      output.contains("newDir") && output.contains("newFile") && !output
        .contains("test")
    )
  }
}
