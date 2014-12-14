package de.famrosner.githubstatistics

import com.google.gson.reflect.TypeToken
import com.google.gson.{JsonObject, JsonArray, Gson}

import scalaj.http.Http
import scalaj.http.HttpResponse

import java.lang.reflect.Type

import scalaz._
import Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

import scala.util.parsing.json._


object Launch {

  case class Repository(val id: Long, val full_name: String)

  class LanguageTypeToken extends TypeToken[java.util.Map[String, Double]]

  val gson = new Gson()
  val languageMapType: Type = new LanguageTypeToken().getType();

  val repositoriesUrl = "https://api.github.com/repositories"

  /**
    *  %s = full_name of repository
   *  Example: https://api.github.com/repos/mojombo/grit/languages
   */
  val languageUrl = "https://api.github.com/repos/%s/languages"

  def main(args: Array[String]): Unit = {

    def repo2language(response: HttpResponse[String]): java.util.Map[String, Double] = {
      gson.fromJson(response.body, languageMapType)
    }

    def mergeMap[A, B](maps: Iterable[Map[A, B]])(aggregate: (B, B) => B): Map[A, B] = {
      val keyValuePairs = for (map <- maps; keyValuePair <- map) yield keyValuePair
      keyValuePairs.foldLeft(Map[A, B]()) { (mergedMap, keyValuePair) =>
        mergedMap + (
          if (mergedMap.contains(keyValuePair._1))
            keyValuePair._1 -> aggregate(mergedMap(keyValuePair._1), keyValuePair._2)
          else
            keyValuePair
        )
      }
    }

    val repositoriesResponse = Http(repositoriesUrl).asString
    val repositories: Array[Repository] = gson.fromJson(repositoriesResponse.body, classOf[Array[Repository]]).take(5)
    val languagesResponse = repositories.map(repository => Http(String.format(languageUrl, repository.full_name)).asString)
    val languagesJava: Iterable[java.util.Map[String, Double]] = languagesResponse.map(repo2language)
    val languagesScala = languagesJava.map(HashMap() ++ _.asScala)
    val languageSummaryBytes = mergeMap(languagesScala)(_ + _)
    val byteSum: Double = languageSummaryBytes.values.sum
    val languageSummaryPercentage = languageSummaryBytes.map{
      case (language, bytes) => (language, bytes / byteSum)
    }
    languageSummaryPercentage foreach println
  }

}
