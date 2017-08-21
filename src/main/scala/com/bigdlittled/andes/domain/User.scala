package com.bigdlittled.andes.domain

// This doesn't make any sense. it should be
// case class User(name: String)
// But this will make the graph work for now with the base node type
// Investment. Obviously I need to refactor that to something sensible
class User(name: String) extends Investment(name)

object User {
  def apply(name: String) = new User(name)
}
