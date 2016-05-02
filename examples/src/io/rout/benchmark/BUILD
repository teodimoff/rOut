scala_library(name = 'benchmark',
  dependencies = [
     'projects3/rOut/generic/src/io/rout/decode:decoding',
     '3rdparty/jvm/com/twitter:finagle-http',
     '3rdparty/jvm/com/twitter:finagle-stats',
     '3rdparty/jvm/com/twitter:twitter-server',
  ],
    sources = globs('package.scala','Payload.scala','AuthFilter.scala','Rout.scala'),
)

scala_library(name = 'benchmark-circe',
  dependencies = [
     '3rdparty/jvm/com/twitter:finagle-http',
     '3rdparty/jvm/com/twitter:finagle-stats',
     'projects3/rOut/circe/src/io/rout/circe',
  ],
    sources = globs('package.scala','Payload.scala','AuthFilter.scala','Circerout.scala'),
)

scala_library(name = 'benchmark-all',
  dependencies = [
     'projects3/rOut/generic/src/io/rout/decode:decoding',
     '3rdparty/jvm/com/twitter:finagle-http',
     '3rdparty/jvm/com/twitter:finagle-stats',
     '3rdparty/jvm/com/twitter:twitter-server',
     'projects3/rOut/circe/src/io/rout/circe',
  ],
    sources = globs('*.scala'),
)

jvm_binary(name='binary',
  basename='rOut-benchmark-rOut-app',
  main='io.rout.benchmark.Rout',
  dependencies=[
  ':benchmark'
  ]
)

jvm_binary(name='binary-circe',
  basename='rOut-benchmark-circe-app',
  main='io.rout.benchmark.CircerOut',
  dependencies=[
  ':benchmark-circe'
  ]
)

jvm_binary(name='binary-all',
  basename='rOut-benchmark-app',
  main='io.rout.benchmark.Benchmark',
  dependencies=[
  ':benchmark-all'
  ]
)