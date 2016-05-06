scala_library(name = 'xml',
  dependencies = [
     '3rdparty/jvm/com/twitter:finagle-http',
     'projects3/rOut/xml/src/io/rout/xml',
   ],
    sources = globs('*.scala'),
) 

jvm_binary(name='binary',
  basename='rOut-benchmark-xml-app',
  main='io.rout.example.xml.Main',
  dependencies=[
  ':xml'
  ]
)