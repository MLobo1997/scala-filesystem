package com.mlobo
package commands

import files.Directory
import filesystem.State
import utils.Path

class Mkdir(path: Path) extends Command {

  def doMkdir(state: State, path: Path): State = {
    val wd = state.wd
    val newDir =
      Directory.empty(
        wd.fullpath.join(path.getParent.toString).getAbsolutePath,
        path.getLast
      )
    state.addEntryToDirectoryTree(newDir)
  }

  def apply(state: State): State = {
    doMkdir(state, path)
  }
}
