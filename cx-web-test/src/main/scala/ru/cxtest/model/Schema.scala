package ru.cxtest.model

import ru.circumflex.orm.Record
import ru.circumflex.orm.Table
import ru.circumflex.orm.SELECT

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
  val timestamp = "timestamp".TIMESTAMP.NOT_NULL
  val pictureUrl = "picture_url".TEXT.NOT_NULL

  def PRIMARY_KEY = id
  def relation = BoobsPost
}

object BoobsPost extends BoobsPost with Table[Long, BoobsPost] {

  // Indexes & constraints
  val timestampIndex = "bp_timestamp_idx".INDEX("timestamp").USING("btree")

  // Data queries
  def getRecent(limit: Int): Seq[BoobsPost] =
    (this AS "bp") map (bp => SELECT(bp.*) FROM (bp) ORDER_BY (bp.timestamp DESC) LIMIT (limit) list)

}

