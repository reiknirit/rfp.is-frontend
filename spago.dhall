{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "rfp-frontend"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "css"
    , "console"
    , "datetime"
    , "dotenv"
    , "effect"
    , "express"
    , "formatters"
    , "halogen"
    , "halogen-css"
    , "halogen-formless"
    , "halogen-media"
    , "precise-datetime"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "strings"
    , "spec"
    , "timestamp"
    ]
, packages =
    ./packages.dhall
}
