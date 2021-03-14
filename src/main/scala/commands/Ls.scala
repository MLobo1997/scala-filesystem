package com.mlobo
package commands
import filesystem.State

class Ls extends Command {
  def apply(state: State): State = {
    val entriesStr: String =
      state.wd.contents
        .map(_.name)
        .fold("")((a, b) => if (a.nonEmpty) a + '\t' + b else b)
    state.setMessage(entriesStr)
  }
}
