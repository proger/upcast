let release = import ./release.nix {}; in
release."${builtins.head (builtins.attrNames release)}".env
