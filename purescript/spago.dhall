{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "test-site"
, dependencies =
    [ "argonaut"
    , "argonaut-generic"
    , "console"
    , "effect"
    , "free"
    , "generics-rep"
    , "heterogeneous"
    , "identity"
    , "prelude"
    , "psci-support"
    , "test-unit"
    , "typelevel"
    , "variant"
    ]
, packages =
    ./packages.dhall
}
