package utils

import com.mongodb.casbah.Imports._
import play.api.Play

object Mongo {

  val mongoUri: String = Play.current.configuration.getString("mongohq.uri").get
  val uri = MongoClientURI(mongoUri)
  val mongoClient = MongoClient(uri)//+"?maxPoolSize=20")

  def getLatLon(place:String): (String, String) = {
    val timer = Timer("mongo.get.latlon")
    val mongoDb = mongoClient.getDB("app30566711")
    val mongoCollection = mongoDb.getCollection("places")
    val dbObj = mongoCollection.findOne(DBObject("place" -> place))
    if (dbObj != null) {
      val lat = dbObj.get("lat").asInstanceOf[String]
      val lon = dbObj.get("lon").asInstanceOf[String]
      timer.logTime()
      (lat, lon)
    } else {
      timer.logTime()
      null
    }
  }

  def addPlace(place:String, lat: String, lon: String): Unit = {
    val mongoUri: String = Play.current.configuration.getString("mongohq.uri").get
    val uri = MongoClientURI(mongoUri)
    val mongoClient = MongoClient(uri)
    val mongoDb = mongoClient.getDB("app30566711")
    val mongoCollection = mongoDb.getCollection("places")
    val newDoc = MongoDBObject("place" -> place) ++ ("lat" -> lat) ++ ("lon" -> lon)
    mongoCollection.insert(newDoc)
  }


}
