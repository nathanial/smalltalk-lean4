import Lake
open Lake DSL

package smalltalk where
  version := v!"0.1.1"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.7"
require sift from git "https://github.com/nathanial/sift" @ "v0.0.4"

@[default_target]
lean_lib Smalltalk where
  roots := #[`Smalltalk]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe smalltalk_tests where
  root := `Tests.Main
