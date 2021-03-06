<!--- Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com> -->
# What's new in Play 2.4

This page highlights the new features of Play 2.4. If you want learn about the changes you need to make to migrate to Play 2.4, check out the [[Play 2.4 Migration Guide|Migration24]].

## Dependency Injection

- TODO(jroper)

## Embeddable Router

- TODO(jroper)
- Java - https://github.com/playframework/playframework/blob/master/documentation/manual/working/javaGuide/advanced/embedding/JavaEmbeddingPlay.md
- Scala - https://github.com/playframework/playframework/blob/master/documentation/manual/working/scalaGuide/advanced/embedding/ScalaEmbeddingPlay.md

## Logging enhancements

- TODO(jroper): jroper made some good changes here with better support for logback
- Log messages generated by Play can now be configured on a per-class basis as opposed to previously needing to change the log level for all Play log messages.

## Java 8 support

- JSON handling now supports Java 8 temporal values such as Instant, LocalDateTime, and LocalDate.
- The Play documentation now shows code examples using Java 8 syntax for callables. As an example, here's how some code has changed.

Before:

    return promise(new Function0<Integer>() {
      public Integer apply() {
        return longComputation();
      }
    }).map(new Function<Integer,Result>() {
      public Result apply(Integer i) {
        return ok("Got " + i);
      }
    });


After:

    return promise(() -> longComputation())
        .map((Integer i) -> ok("Got " + i));


## Java Testing

A new `RequestBuilder` replaces `FakeRequest` and `RequestBuilder` will build a request that is actually a `Request`. `FakeRequest` was not a `Request` which meant it was not interopable with many of the Play APIs. This improvement makes it much easier to instantiate a controller and call a method on it during testing.

## Maven/SBT standard layout

- Play will now let you use either its default layout or the directory layout that is the default for Maven and SBT projects. See the [[Anatomy of a Play application|Anatomy]] page for more details.

## Anorm

- New positional getter on `Row`.
- Unified column resolution by label, whatever it is (name or alias).
- New streaming API; Functions `fold` and `foldWhile` to work with result stream (e.g. `SQL"Select count(*) as c from Country".fold(0l) { (c, _) => c + 1 }`). Function `withResult` to provide custom stream parser (e.g. `SQL("Select name from Books").withResult(customTailrecParser(_, List.empty[String]))`).
- Supports array (`java.sql.Array`) from column (e.g. `SQL("SELECT str_arr FROM tbl").as(scalar[Array[String]].*)`) or as parameter (e.g. `SQL"""UPDATE Test SET langs = ${Array("fr", "en", "ja")}""".execute()`).
- Improved conversions for numeric and boolean columns.
- New conversions for binary columns (bytes, stream, blob), to parsed them as `Array[Byte]` or `InputStream`.
- New conversions for Joda `Instant` or `DateTime`, from `Long`, `Date` or `Timestamp` column.
- Added conversions to support `List[T]`, `Set[T]`, `SortedSet[T]`, `Stream[T]` and `Vector[T]` as multi-value parameter.
- New conversion to parse text column as UUID (e.g. `SQL("SELECT uuid_as_text").as(scalar[java.util.UUID].single)`).

## HikariCP

[HikariCP](http://brettwooldridge.github.io/HikariCP/) is now the default JDBC connection pool. It's properties can be directly configured using `.conf` files and you should rename the configuration properties to match what is expected by HikariCP.