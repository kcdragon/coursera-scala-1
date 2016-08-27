package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("set with a single element that does match") {
    new TestSets {
      assert(size(set2.filter(tw => tw.retweets == 20)) === 1)
    }
  }

  test("set with a single element that does not match") {
    new TestSets {
      assert(size(set2.filter(tw => tw.retweets == 0)) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union non-empty set and an empty set is the non-empty set") {
    new TestSets {
      assert(size(set2.union(set1)) === 1)
    }
  }

  test("union empty set and a non-empty set is the non-empty set") {
    new TestSets {
      assert(size(set1.union(set2)) === 1)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retweeted of single element set is that element") {
    new TestSets {
      assert(set2.mostRetweeted === new Tweet("a", "a body", 20))
    }
  }

  test("most retweeted of multiple elements is element with highest retweets (1)") {
    new TestSets {
      val tweetOne = new Tweet("a", "a body", 1)
      val tweetTwo = new Tweet("b", "b body", 2)
      val set = (new Empty).incl(tweetOne).incl(tweetTwo)
      assert(set.mostRetweeted === tweetTwo)
    }
  }

  test("most retweeted of multiple elements is element with highest retweets (2)") {
    new TestSets {
      val tweetOne = new Tweet("a", "a body", 2)
      val tweetTwo = new Tweet("b", "b body", 1)
      val set = (new Empty).incl(tweetOne).incl(tweetTwo)
      assert(set.mostRetweeted === tweetOne)
    }
  }

  test("descending: no element set returns no elements") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }

  test("descending: single element set returns that element") {
    new TestSets {
      val trends = set2.descendingByRetweet
      assert(trends.head.user === "a")
    }
  }

  test("descending: two elements out of order") {
    new TestSets {
      val tweetOne = new Tweet("a", "a body", 1)
      val tweetTwo = new Tweet("b", "b body", 2)
      val set = (new Empty).incl(tweetOne).incl(tweetTwo)
      val trends = set.descendingByRetweet
      assert(trends.head.user == "b")
      assert(trends.tail.head.user == "a")
    }
  }

  test("descending: two elements in order") {
    new TestSets {
      val tweetOne = new Tweet("a", "a body", 2)
      val tweetTwo = new Tweet("b", "b body", 1)
      val set = (new Empty).incl(tweetOne).incl(tweetTwo)
      val trends = set.descendingByRetweet
      assert(trends.head.user == "a")
      assert(trends.tail.head.user == "b")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("appleTweets: contains apple tweets") {
    val appleTweet = new Tweet("gizmodo", "These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp", 49)
    assert(GoogleVsApple.appleTweets.contains(appleTweet))
  }

  test("appleTweets: does not contain google tweets") {
    val googleTweet = new Tweet("gizmodo", "Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290)
    assert(!GoogleVsApple.appleTweets.contains(googleTweet))
  }

  test("googleTweets: does not contain apple tweets") {
    val appleTweet = new Tweet("gizmodo", "These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp", 49)
    assert(!GoogleVsApple.googleTweets.contains(appleTweet))
  }

  test("googleTweets: contains google tweets") {
    val googleTweet = new Tweet("gizmodo", "Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw", 290)
    assert(GoogleVsApple.googleTweets.contains(googleTweet))
  }

  // test("trending") {
  //   GoogleVsApple.trending foreach println
  // }

}
