let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [  "assert" , "console" ,
    "node-buffer" , "node-fs-aff", "test-unit" , "quickcheck" ]
}
