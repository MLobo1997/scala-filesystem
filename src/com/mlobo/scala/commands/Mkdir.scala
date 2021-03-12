package com.mlobo.scala.commands
import com.mlobo.scala.files.Directory
import com.mlobo.scala.filesystem.State

class Mkdir(name: String) extends Command {

  def doMkdir(state: State, name: String): State = {
    val wd = state.wd
    val newDir = Directory.empty(wd.fullpath, name)
    state.addEntryToDirectoryTree(newDir, newDir.fullpath)
  }

  def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {} else if (
      checkIllegal(name)
    ) {} else {}

    (wd, name) match {
      case (wd, name) if wd.hasEntry(name) =>
        state.setMessage(s"Entry $name already exists!")
      case (_, name) if name.contains(Directory.SEPARATOR) =>
        state.setMessage(s"$name must not contain separators")
      case (_, name) if checkIllegal(name) =>
        state.setMessage(s"$name: illegal entry name")
      case _ =>
        doMkdir(state, name)
    }
  }

  def checkIllegal(name: String): Boolean =
    name.contains('.')
}
