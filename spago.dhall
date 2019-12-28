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
    , "halogen-formless"
    , "precise-datetime"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "strings"
    , "spec"
    ]
, packages =
    ./packages.dhall
}
