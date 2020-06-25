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
    , "b64"
    , "css"
    , "console"
    , "datetime"
    , "dotenv"
    , "effect"
    , "encoding"
    , "express"
    , "formatters"
    , "halogen"
    , "halogen-css"
    , "halogen-formless"
    , "halogen-media"
    , "halogen-rawhtml"
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
