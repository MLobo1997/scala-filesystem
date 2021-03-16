package com.mlobo
package commands

import com.mlobo.files.Directory
import com.mlobo.filesystem.State
import com.mlobo.utils.Path
import org.scalatest.funsuite.AnyFunSuite

class LsTest extends AnyFunSuite {
  val TEST_DIR: Directory = Directory.empty(Path("/"), "test")

  test("Identifies directories in root") {
    val root = Directory(Path(""), "", List(TEST_DIR))
    val state = State(root, root)
    assert(new Ls()(state).output.contains("test"))
  }

  test("Identifies directories in wd") {
    val root = Directory(Path(""), "", List(TEST_DIR))
    val wd = TEST_DIR
      .addNewEntry(Directory.empty(TEST_DIR.fullpath, "test1"))
      .get
    val state = State(root, wd)
    assert(new Ls()(state).output.contains("test1"))
  }
}
