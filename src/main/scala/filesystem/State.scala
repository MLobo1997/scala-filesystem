package com.mlobo
package filesystem

import files.{DirEntry, Directory}

import scala.util.{Failure, Success}

class State(val root: Directory, val wd: Directory, val output: String) {
  def show(): Unit = {
    println(output)
    print(State.SHELL_TOKEN)
  }

  def setMessage(message: String): State =
    State(root, wd, message)

  def addEntryToDirectoryTree(entry: DirEntry, fullPath: String): State = {
    root.addEntryWithPath(entry, fullPath) match {
      case Success(newRoot) =>
        State(
          newRoot,
          newRoot.getDirectoryWithPath(wd.fullpath, wd.name).get,
          output = s"Added ${entry.name}"
        )
      case Failure(exception) =>
        State(root, wd, output = exception.getMessage)
    }
  }
}

object State {
  val SHELL_TOKEN = "$ "

  def apply(root: Directory, wd: Directory, output: String = ""): State =
    new State(root, wd, output)
}
