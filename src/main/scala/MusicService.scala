package com.music

import scala.annotation.tailrec


trait MusicService {

  import MusicService._

  private var aliasIndex = Map.empty[String, Artist]
  private var artistOfTheDayCache: List[Artist] = Nil

  def addTrack(artistName: String, track: Track): Unit = {
    val artist: Artist = aliasIndex.getOrElse(artistName, createNewArtist(artistName))

    val updatedArtist = artist.copy(
      tracks = artist.tracks + track
    )

    updateAliasIndex(updatedArtist)
  }

  def fetchArtistTracks(artistAlias: String): Set[Track] = aliasIndex.get(artistAlias).map(_.tracks).getOrElse(Set.empty)

  def editArtistNames(nameOrAliasToReplace: String, allNewNames: Set[String]): Unit = {
    val artist = aliasIndex.getOrElse(nameOrAliasToReplace, createNewArtist(nameOrAliasToReplace))

    val updatedArtist = artist.copy(
      artistAliases = allNewNames
    )

    aliasIndex --= artist.artistAliases

    updateAliasIndex(updatedArtist)
  }

  @tailrec
  final def artistOfTheDay: Option[String] = artistOfTheDayCache match {
    case Nil => {
      artistOfTheDayCache = aliasIndex.values.toSet.toList
      if (artistOfTheDayCache.isEmpty) None else artistOfTheDay
    }
    case head :: rest => {
      artistOfTheDayCache = rest

      val candidate = head.artistAliases.find(aliasIndex.contains)
      if (candidate.isDefined) candidate
      else artistOfTheDay
    }
  }

  private def updateAliasIndex(artist: Artist): Unit = aliasIndex ++= artist.artistAliases.map(name => name -> artist).toMap

}

object MusicService {
  case class Track(title: String, genre: String, length: Int)

  private case class Artist(artistAliases: Set[String], tracks: Set[Track] = Set.empty)

  private def createNewArtist(artistName: String): Artist = {
    Artist(Set(artistName))
  }
}
