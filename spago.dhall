{ name = "purrs"
, dependencies = 
  [ "console"
  , "effect"
  , "psci-support"
  , "tuples"
  , "arrays"
  , "integers"
  , "partial"
  , "maybe"
  , "exceptions"
  , "spec"
  , "foldable-traversable"
  , "transformers"
  , "spec-discovery"
  , "debug"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
