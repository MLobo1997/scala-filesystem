package com.mlobo
package commands

import filesystem.State
import utils.Path

import org.scalatest.funsuite.AnyFunSuite

class CommandsTest extends AnyFunSuite {
  test("Mkdir, cd and ls") {
    val s1 = State.clean
    assert(new Ls()(s1).output.isEmpty)
    val s2 = new Mkdir(Path("test"))(s1)
    assert(new Ls()(s2).output.contains("test"))
    val s3 = new Cd(Path("test"))(s2)
    assert(new Ls()(s3).output.isEmpty)
    val s4 = new Touch("newFile")(new Mkdir(Path("newDir"))(s3))
    val output = new Ls()(s4).output
    assert(
      output.contains("newDir") && output.contains("newFile") && !output
        .contains("test")
    )
    val s5 = new Mkdir(Path("../relativeDir"))(s4)
    assert(s5.root.getDirectory("relativeDir").isSuccess)
  }

  test("Mkdir and cd") {
    val s1 = new Mkdir(Path("t1/t2"))(new Mkdir(Path("t1"))(State.clean))
    assert(new Ls()(s1).output.contains("t1"))
    val s2 = new Cd(Path("t1/t2"))(s1)
    assert(new Ls()(s2).output.isEmpty)
    val s3 = new Cd(Path("../.."))(s2)
    assert(new Ls()(s3).output.contains("t1"))
  }
}
