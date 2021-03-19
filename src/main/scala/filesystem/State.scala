package com.mlobo
package filesystem

import files.{DirEntry, Directory}

import com.mlobo.utils.Path

import scala.util.{Failure, Success, Try}

class State(val root: Directory, val wd: Directory, val output: String) {
  def show(): Unit = {
    println(output)
    print(s"${wd.fullpath} ${State.SHELL_TOKEN}")
  }

  def setMessage(message: String): State =
    State(root, wd, message)

  private def updateDirectoryTree(
      maybeNewRoot: Try[Directory],
      successOutputMessage: String
  ): State = {
    maybeNewRoot match {
      case Failure(exception) => setMessage(exception.getMessage)
      case Success(newRoot) =>
        State(
          newRoot,
          newRoot.getDirectoryWithRelativePath(wd).get,
          output = successOutputMessage
        )
    }
  }

  def removeEntryFromDirectoryTree(path: Path): State =
    updateDirectoryTree(root.removeEntryRelativePath(path), s"Removed $path")

  def addEntryToDirectoryTree(entry: DirEntry): State =
    updateDirectoryTree(
      root.addEntryInRelativePath(entry),
      s"Added ${entry.name}"
    )
}

object State {
  val SHELL_TOKEN = "$ "

  def clean: State =
    State(Directory.ROOT, Directory.ROOT)

  def apply(root: Directory, wd: Directory, output: String = ""): State =
    new State(root, wd, output)
}
