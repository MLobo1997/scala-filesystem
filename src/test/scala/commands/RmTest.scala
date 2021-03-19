package com.mlobo
package commands

import filesystem.State
import utils.Path

import org.scalatest.funsuite.AnyFunSuite

class RmTest extends AnyFunSuite {
  test("Remove a directory") {
    val s = new Rm(Path("test"))(new Mkdir(Path("test"))(State.clean))
    assert(
      !s.root.contents.exists(_.name.equals("test")) &&
        !s.wd.contents.exists(_.name.equals("test"))
    )
  }

  test("Remove a relative directory") {
    val s = new Mkdir(Path("coiso"))(new Mkdir(Path("test"))(State.clean))
    val s2 = new Rm(Path("../coiso"))(new Cd(Path("test"))(s))
    assert(
      !s2.root.contents.exists(_.name.equals("coiso"))
    )
  }
}
