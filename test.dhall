let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # 
  [ "aff"
  , "exceptions"
  , "node-buffer" 
  , "node-path"
  , "node-fs-aff"
  , "nonempty"
  , "spec"
  , "spec-quickcheck" 
  , "transformers"
  , "quickcheck" 
  ]
}
