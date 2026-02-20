/* Template from: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#scaffolding

   List of supported systems for cross-compilation: https://github.com/NixOS/nixpkgs/blob/master/lib/systems/examples.nix

    nix flake metadata
    nix flake show --allow-import-from-derivation
*/

{
  description = "A very basic flake";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    nix-filter.url = "github:numtide/nix-filter";

    # haskell
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-filter }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        filter = nix-filter.lib;

        overlays = [
          haskellNix.overlay
          (final: _prev: {
            # This overlay adds our project to pkgs
            twenty48Project = final.haskell-nix.project' {

              # We explicitly list out every file needed to build the project
              # to avoid rebuilds when files like `README.md` or `justfile` change.
              # Related:
              #   https://github.com/numtide/nix-filter
              #   https://discourse.nixos.org/t/how-to-make-src-in-a-flake-nix-not-change-a-lot/15129
              #   https://discourse.nixos.org/t/excluding-a-subdirectory-when-using-local-paths/3954
              #   https://unix.stackexchange.com/q/720616/98391
              src = filter {
                root = ./.;
                include = [
                  "src"
                  "app"
                  "config"
                  "static"
                  "templates"
                  ./package.yaml
                  ./stack.yaml
                  ./stack.yaml.lock
                ];
              };

              # Use stack.yaml instead of cabal.project
              projectFileName = "stack.yaml";
              compiler-nix-name = "ghc9103";

              # Workaround for: https://github.com/input-output-hk/haskell.nix/issues/2423
              modules = [
                {
                  packages.directory.flags.os-string = true;
                  packages.unix.flags.os-string = true;
                }
                {
                  /* This is a workaround for the fact that `hlibsass` adds an RPATH to the library, which causes it to fail to load when the build directory is deleted after the build.
                      The `dontPatchELF` option prevents `haskell.nix` from patching the ELF files, and the `postInstall` script removes the RPATH from the library.

                     > Running phase: installPhase
                     > Installing library in /nix/store/jc9h0d4ss2gpwgj37r67gjvwkpmymnr6-hlibsass-lib-hlibsass-0.1.10.3/lib/x86_64-linux-ghc-9.10.3-inplace/hlibsass-0.1.10.3-DKAnadfp9Xk3jxXQ3VsTNl
                     > Running phase: fixupPhase
                     > checking for references to /build/ in /nix/store/jc9h0d4ss2gpwgj37r67gjvwkpmymnr6-hlibsass-lib-hlibsass-0.1.10.3...
                     > patchelf: wrong ELF type
                     > RPATH of binary /nix/store/jc9h0d4ss2gpwgj37r67gjvwkpmymnr6-hlibsass-lib-hlibsass-0.1.10.3/lib/x86_64-linux-ghc-9.10.3-inplace/libHShlibsass-0.1.10.3-DKAnadfp9Xk3jxXQ3VsTNl-ghc9.10.3.so contains a forbidden reference to /build/
                     > Some binaries contain forbidden references to /build/. Check the error above!

                     Here's what was going on:
                       * `hlibsass` builds a shared library (`libHShlibsass*.so`) and embeds an RPATH that includes the temporary build directory, typically something like `/build/...`
                       * In Nix builds, `/build` is a sandbox path that must never leak into final outputs. The `fixupPhase` runs a “reference scanner” that checks every ELF for forbidden references like `/build`.
                       * The scanner detects that `/build` is still present in the library's RPATH and tries to clean it using `patchelf`.
                       * That `patchelf` call fails with “wrong ELF type”, so the RPATH never gets cleaned, and the reference check fails the build.
                         * The “wrong ELF type” here is `patchelf` refusing to operate on that specific file.
                           In practice, this can happen when the file is not a regular shared object (e.g., a GHC “inplace” library layout,
                           a linker-script-like file, or a format patchelf doesn't expect).
                           Either way, the default fixup path can't patch it.
                       * Because the build dir reference remains, the Nix store validation fails with: “RPATH … contains a forbidden reference to /build/”.

                       The fix works by:
                       * Preventing the default ELF patching for that component (dontPatchELF = true), which avoids the failing patchelf call.
                       * Manually removing the RPATH from the actual shared objects after install, so the /build reference is gone before the reference checker runs.

                       So the root cause is the combination of:
                       * a build-time RPATH leak in hlibsass's shared library, and
                       * `patchelf` being unable to patch that file during fixup, leaving `/build` references intact.
                  */
                  packages.hlibsass.components.library = {
                    # Strip the build dir RPATH to avoid /build references.
                    dontPatchELF = true;
                    postInstall = ''
                      while IFS= read -r -d "" f; do
                        if [ "$(head -c 4 "$f")" = "$(printf '\x7fELF')" ]; then
                          patchelf --remove-rpath "$f"
                        fi
                      done < <(find "$out" -type f -name 'libHShlibsass*.so*' -print0)
                    '';
                  };
                }
              ];

              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = { };
                # hlint = {};
                # haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
      in {
        packages = {
          # Build the server
          twenty48 =
            (pkgs.twenty48Project.flake { }).packages."twenty48:exe:twenty48";

        };
      });
}
