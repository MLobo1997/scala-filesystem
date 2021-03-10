package com.mlobo.scala.commands

import com.mlobo.scala.filesystem.State

trait Command {
  def apply(state: State): State
}

object Command {
  def from(input: String): Command =
    new UnknownCommand
}
