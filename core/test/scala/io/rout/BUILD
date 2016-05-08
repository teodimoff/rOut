junit_tests(name = 'rout',
  dependencies = [
     ':resources',
     '3rdparty:junit',
     '3rdparty/jvm/org/typelevel:cats-core',
     '3rdparty/jvm/org/typelevel:cats-laws',
     '3rdparty/jvm/org/typelevel:discipline',
     '3rdparty/jvm/org/scalatest',
     '3rdparty/jvm/org/scalacheck',
     '3rdparty/jvm/com/chuusai/shapeless:shapeless',
     '3rdparty/jvm/com/twitter:finagle-http',
     '3rdparty/jvm/com/twitter:util-eval',
     'projects3/rOut/core/src/io:rout',
  ],
    sources = globs('*.scala','internal/*.scala'),
)

scala_library(name = 'resources',
   resources = [
   'projects3/rOut/core/test/resources:upload',
   ],
)