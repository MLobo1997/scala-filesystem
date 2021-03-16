package com.mlobo
package commands
import files.File
import filesystem.State

class Touch(val name: String) extends Command {
  def apply(state: State): State = {
    val file: File = new File(state.wd.fullpath, name)
    state.addEntryToDirectoryTree(file)
  }
}
