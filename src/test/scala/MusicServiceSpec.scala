package com.music

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import com.music.MusicService.*
import org.scalatest.BeforeAndAfterEach

class MusicServiceSpec extends AnyFunSpec with Matchers with BeforeAndAfterEach {

  var service: MusicService = _

  override def beforeEach(): Unit = {
    service = new MusicService {}
  }

  describe("Track operations") {
    it("Add new Track") {
      service.addTrack("Artist1", Track("title1", "genre1", 5))

      service.fetchArtistTracks("Artist1") shouldBe Set(Track("title1", "genre1", 5))
    }

    it("Get tracks for not existing artist") {
      service.fetchArtistTracks("Absent Artist") shouldBe Set.empty
    }

    it("Add more than one track") {
      service.addTrack("Artist1", Track("title1", "genre1", 5))
      service.addTrack("Artist1", Track("title2", "genre2", 3))

      service.fetchArtistTracks("Artist1") shouldBe Set(
        Track("title1", "genre1", 5),
        Track("title2", "genre2", 3)
      )
    }

    it("Add track to all of the aliases") {
      service.editArtistNames("Artist1", Set("Artist1", "alias1", "alias2"))
      service.addTrack("Artist1", Track("title1", "genre1", 6))

      service.fetchArtistTracks("Artist1") shouldBe Set(Track("title1", "genre1", 6))
      service.fetchArtistTracks("alias1") shouldBe Set(Track("title1", "genre1", 6))
      service.fetchArtistTracks("alias2") shouldBe Set(Track("title1", "genre1", 6))
    }
  }
  describe("Artist operations") {

    describe("Add alias to Artist") {
      it("Absent Artist") {
        service.editArtistNames("Absent Artist", Set("AnotherName1", "AnotherName2"))

        service.fetchArtistTracks("Absent Artist") shouldBe Set.empty
        service.fetchArtistTracks("AnotherName1") shouldBe Set.empty
        service.fetchArtistTracks("AnotherName2") shouldBe Set.empty
      }

      it("Existing Artist") {
        service.addTrack("NotEmptyArtist", Track("title1", "genre1", 8))
        service.editArtistNames("NotEmptyArtist", Set("AnotherName5", "AnotherName6"))

        service.fetchArtistTracks("NotEmptyArtist") shouldBe Set.empty
        service.fetchArtistTracks("AnotherName5") shouldBe Set(Track("title1", "genre1", 8))
        service.fetchArtistTracks("AnotherName6") shouldBe Set(Track("title1", "genre1", 8))
      }

      it("Remove all names") {
        service.addTrack("Some Artist", Track("title1", "genre1", 9))
        service.editArtistNames("NotEmptyArtist", Set.empty)

        service.fetchArtistTracks("NotEmptyArtist") shouldBe Set.empty
      }
    }
  }
  describe("Artist of the day") {
    it("Shows all of the artists fairly") {
      service.addTrack("Some Artist 1", Track("title1", "genre1", 9))
      service.addTrack("Some Artist 2", Track("title1", "genre1", 9))
      service.addTrack("Some Artist 3", Track("title1", "genre1", 9))

      Set(service.artistOfTheDay.get, service.artistOfTheDay.get, service.artistOfTheDay.get) shouldBe Set("Some Artist 1", "Some Artist 2", "Some Artist 3")
      Set(service.artistOfTheDay.get, service.artistOfTheDay.get, service.artistOfTheDay.get) shouldBe Set("Some Artist 1", "Some Artist 2", "Some Artist 3")
    }

    it("Empty db") {
      service.artistOfTheDay shouldBe None
    }

    describe("Doesn't show artists that doesn't have names") {
      it("Removed before activation") {
        service.addTrack("Some Artist 1", Track("title1", "genre1", 9))
        service.editArtistNames("Some Artist 1", Set.empty)

        service.artistOfTheDay shouldBe None
      }

      it("Removed during activation") {
        service.addTrack("Some Artist 1", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 2", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 3", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 4", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 5", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 6", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 7", Track("title1", "genre1", 9))
        service.addTrack("Some Artist 8", Track("title1", "genre1", 9))

        val initialSet = Set("Some Artist 1", "Some Artist 2", "Some Artist 3", "Some Artist 4", "Some Artist 5", "Some Artist 6", "Some Artist 7", "Some Artist 8")

        val setAfterActivation = initialSet -- service.artistOfTheDay

        val remove :: artistsOfDaysToShow = setAfterActivation.toList: @unchecked

        service.editArtistNames(remove, Set.empty)

        artistsOfDaysToShow.map(_ => service.artistOfTheDay.get).toSet shouldBe artistsOfDaysToShow.toSet
      }
    }
  }
}