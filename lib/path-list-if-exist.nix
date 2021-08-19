# Input: a path
# Output: a list with the path if exists, empty list if it isn't exist
path:
let
  path-as-path = /. + path;
  is-exist = builtins.pathExists path;
in
if is-exist then [ path-as-path ] else [ ]
