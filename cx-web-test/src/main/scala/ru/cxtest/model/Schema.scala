package ru.cxtest.model

import ru.circumflex.orm.Record
import ru.circumflex.orm.Table

class User extends Record[Int, User] {

  val id = "id".INTEGER.NOT_NULL.AUTO_INCREMENT
  val userName = "user_name".TEXT.NOT_NULL

  def PRIMARY_KEY = id
  def relation = User
}

object User extends User with Table[Int, User]

class BoobsPost extends Record[Long, BoobsPost] {

  val id = "id".BIGINT.NOT_NULL.AUTO_INCREMENT
  val poster = "user_id".INTEGER.REFERENCES(User)

  def PRIMARY_KEY = id
  def relation = BoobsPost
}

object BoobsPost extends BoobsPost with Table[Long, BoobsPost]

