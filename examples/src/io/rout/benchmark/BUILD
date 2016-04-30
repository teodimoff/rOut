scala_library(name = 'benchmark',
  dependencies = [
     'projects3/rOut/generic/src/io/rout/decode:decoding',
     '3rdparty/jvm/com/twitter:finagle-http',
     '3rdparty/jvm/com/twitter:finagle-stats',
     '3rdparty/jvm/com/twitter:twitter-server',
  ],
    sources = globs('*.scala'),
) 

jvm_binary(name='binary',
  basename='rOut-benchmark-app',
  main='io.rout.benchmark.Main',
  dependencies=[
  ':benchmark'
  ]
)