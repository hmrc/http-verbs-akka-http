http-verbs-play-25
==========

[![Build Status](https://travis-ci.org/hmrc/http-verbs-play-25 .svg)](https://travis-ci.org/hmrc/http-verbs-play-25 ) [ ![Download](https://api.bintray.com/packages/hmrc/releases/http-verbs-play-25 /images/download.svg) ](https://bintray.com/hmrc/releases/http-verbs-play-25 /_latestVersion)

http-verbs-play-25 is a Scala library providing concrete implementation of [hrmc/http-core](https://github.com/hmrc/http-core) for making asynchronous HTTP calls. The underlying implementation uses [Play 2.5 WS](https://www.playframework.com/documentation/2.5.x/ScalaWS).

It encapsulates some common concerns for calling other HTTP services on the HMRC Tax Platform, including:

* ~~Auditing~~
* Logging
* Propagation of common headers
* Response handling, converting failure status codes into a consistent set of exceptions - allows failures to be automatically propagated to the caller
* Request & Response de-serialization

**Auditing is no longer part of http-verbs, please see docs for [play-auditing](http://github.com/hmrc/play-auditing) for further info.**

## Adding to your build

In your SBT build add:

```scala
resolvers += Resolver.bintrayRepo("hmrc", "releases")

// is needed in your project to get the implementation for http transport
libraryDependencies += "uk.gov.hmrc" %% "http-verbs-play-25" % "x.x.x"

// is needed in your project to get the implementation of the business logic 
libraryDependencies += "uk.gov.hmrc" %% "http-verbs" % "x.x.x" 
```

## Usage

All examples are available here:[hmrc/http-verbs-example](https://github.com/hmrc/http-verbs-example)  



## License ##
 
This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
