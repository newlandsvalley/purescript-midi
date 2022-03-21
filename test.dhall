let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # 
  [ "free"
  , "node-buffer" 
  , "node-path"
  , "node-fs-aff"
  , "nonempty"
  , "test-unit" 
  , "quickcheck" 
  ]
}
