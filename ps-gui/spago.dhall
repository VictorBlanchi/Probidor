{ name = "ps-gui"
, dependencies = [ "console", "effect", "halogen", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
