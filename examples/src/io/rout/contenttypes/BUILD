scala_library(name = 'contenttypes',
  dependencies = [
     'projects3/rOut/generic/src/io/rout/decode:decoding',
     'projects3/rOut/circe/src/io/rout/circe',
     '3rdparty/jvm/com/twitter:finagle-http',
     '3rdparty/jvm/com/twitter:finagle-stats',
     '3rdparty/jvm/com/twitter:twitter-server',
  ],
    sources = globs('*.scala'),
) 

jvm_binary(name='binary',
  basename='rOut-app',
  main='io.rout.auth.Main',
  dependencies=[
  ':auth'
  ]
)