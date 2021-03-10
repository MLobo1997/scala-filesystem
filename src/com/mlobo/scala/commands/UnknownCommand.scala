package com.mlobo.scala.commands
import com.mlobo.scala.filesystem.State

class UnknownCommand extends Command {
  def apply(state: State): State = state.setMessage("Command not found!")

}
