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
          (final: prev: {
            # For cross builds, PostgreSQL failed with clang wrapper exec-format
            # errors and lib/dev output reference cycles. We force GCC + native
            # binutils, drop LTO, and collapse PostgreSQL to a single output
            # while regenerating pg_config.env so downstream Haskell deps can
            # find libpq via the pg_config wrapper.
            postgresql =
              if final.stdenv.buildPlatform != final.stdenv.hostPlatform then
                prev.postgresql.overrideAttrs (old: {
                  configureFlags = (final.lib.filter (flag:
                    !(final.lib.hasPrefix "--libdir=" flag
                      || final.lib.hasPrefix "--includedir=" flag))
                    (old.configureFlags or [ ]))
                    ++ [ "--libdir=$out/lib" "--includedir=$out/include" ];
                  outputs = [ "out" ];
                  patches = final.lib.filter (patch:
                    !(final.lib.hasInfix "paths-for-split-outputs"
                      (builtins.baseNameOf patch))) (old.patches or [ ]);
                  prePatch = (old.prePatch or "") + ''
                    export dev="$out"
                  '';
                  preFixup = (old.preFixup or "") + ''
                    export dev="$out"
                  '';
                  postInstall = ''
                    # Single-output build: keep artifacts in $out and
                    # generate pg_config.env for the pg_config wrapper.
                    make -C src/common pg_config.env
                    substituteInPlace src/common/pg_config.env \
                      --replace-fail "$out/share/man" "@man@/share/man" \
                      --replace-fail "$out" "@out@"
                    install -D src/common/pg_config.env "$out/nix-support/pg_config.env"
                    # Ensure libpq headers are present for downstream builds.
                    if [ -d src/include ] && [ -d src/interfaces/libpq ]; then
                      mkdir -p "$out/include"
                      cp -R src/include/. "$out/include/"
                      cp -R src/interfaces/libpq/*.h "$out/include/" || true
                    fi
                    if [ -d src/interfaces/libpq ]; then
                      mkdir -p "$out/lib"
                      cp -P src/interfaces/libpq/libpq.so* "$out/lib/" 2>/dev/null || true
                      cp -P src/interfaces/libpq/libpq.a "$out/lib/" 2>/dev/null || true
                    fi
                    if [ -d "$out/include" ]; then
                      while IFS= read -r -d "" link; do
                        target="$(readlink "$link")"
                        if [ "''${target#/}" = "$target" ]; then
                          resolved="$(cd "$(dirname "$link")" && readlink -f "$target" 2>/dev/null || true)"
                        else
                          resolved="$target"
                        fi
                        if [ -z "$resolved" ] || [ ! -e "$resolved" ]; then
                          case "$target" in
                            /build/source/*)
                              resolved="$PWD/''${target#/build/source/}"
                              ;;
                            /nix/store/src/*)
                              resolved="$PWD/src/''${target#/nix/store/src/}"
                              ;;
                          esac
                        fi
                        if [ -e "$resolved" ]; then
                          rm "$link"
                          cp "$resolved" "$link"
                        fi
                      done < <(find "$out/include" -type l -print0)
                      for spec in \
                        "utils/probes.h:src/backend/utils/probes.h" \
                        "utils/errcodes.h:src/backend/utils/errcodes.h" \
                        "utils/fmgroids.h:src/backend/utils/fmgroids.h" \
                        "utils/fmgrprotos.h:src/backend/utils/fmgrprotos.h" \
                        "nodes/nodetags.h:src/backend/nodes/nodetags.h" \
                        "pg_config_os.h:src/include/port/linux.h"; do
                        rel="''${spec%%:*}"
                        src="''${spec##*:}"
                        if [ -e "$out/include/$rel" ] || [ -L "$out/include/$rel" ]; then
                          rm -f "$out/include/$rel"
                        fi
                        if [ -e "$PWD/$src" ]; then
                          mkdir -p "$(dirname "$out/include/$rel")"
                          cp "$PWD/$src" "$out/include/$rel"
                        fi
                      done
                    fi
                  '';
                  outputChecks = { };
                  separateDebugInfo = false;
                  nativeBuildInputs = (old.nativeBuildInputs or [ ])
                    ++ [ prev.buildPackages.gcc prev.buildPackages.binutils ];
                  preConfigure = (old.preConfigure or "") + ''
                    export PATH="${prev.buildPackages.gcc}/bin:${prev.buildPackages.binutils}/bin:$PATH"
                    export CC="${prev.buildPackages.gcc}/bin/${final.stdenv.cc.targetPrefix}gcc"
                    export CXX="${prev.buildPackages.gcc}/bin/${final.stdenv.cc.targetPrefix}g++"
                    export AR="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ar"
                    export RANLIB="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ranlib"
                    export LD="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ld"
                    export READELF="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}readelf"
                    export STRIP="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}strip"
                    export OBJCOPY="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}objcopy"
                    export CFLAGS="$(echo "$CFLAGS" | sed -e 's/-flto//g')"
                    export NIX_CFLAGS_COMPILE="$(echo "$NIX_CFLAGS_COMPILE" | sed -e 's/-flto//g')"
                  '';
                })
              else
                prev.postgresql;

            postgresql_17 =
              if final.stdenv.buildPlatform != final.stdenv.hostPlatform then
                prev.postgresql_17.overrideAttrs (old: {
                  configureFlags = (final.lib.filter (flag:
                    !(final.lib.hasPrefix "--libdir=" flag
                      || final.lib.hasPrefix "--includedir=" flag))
                    (old.configureFlags or [ ]))
                    ++ [ "--libdir=$out/lib" "--includedir=$out/include" ];
                  outputs = [ "out" ];
                  patches = final.lib.filter (patch:
                    !(final.lib.hasInfix "paths-for-split-outputs"
                      (builtins.baseNameOf patch))) (old.patches or [ ]);
                  prePatch = (old.prePatch or "") + ''
                    export dev="$out"
                  '';
                  preFixup = (old.preFixup or "") + ''
                    export dev="$out"
                  '';
                  postInstall = ''
                    # Single-output build: keep artifacts in $out and
                    # generate pg_config.env for the pg_config wrapper.
                    make -C src/common pg_config.env
                    substituteInPlace src/common/pg_config.env \
                      --replace-fail "$out/share/man" "@man@/share/man" \
                      --replace-fail "$out" "@out@"
                    install -D src/common/pg_config.env "$out/nix-support/pg_config.env"
                    # Ensure libpq headers are present for downstream builds.
                    if [ -d src/include ] && [ -d src/interfaces/libpq ]; then
                      mkdir -p "$out/include"
                      cp -R src/include/. "$out/include/"
                      cp -R src/interfaces/libpq/*.h "$out/include/" || true
                    fi
                    if [ -d src/interfaces/libpq ]; then
                      mkdir -p "$out/lib"
                      cp -P src/interfaces/libpq/libpq.so* "$out/lib/" 2>/dev/null || true
                      cp -P src/interfaces/libpq/libpq.a "$out/lib/" 2>/dev/null || true
                    fi
                    if [ -d "$out/include" ]; then
                      while IFS= read -r -d "" link; do
                        target="$(readlink "$link")"
                        if [ "''${target#/}" = "$target" ]; then
                          resolved="$(cd "$(dirname "$link")" && readlink -f "$target" 2>/dev/null || true)"
                        else
                          resolved="$target"
                        fi
                        if [ -z "$resolved" ] || [ ! -e "$resolved" ]; then
                          case "$target" in
                            /build/source/*)
                              resolved="$PWD/''${target#/build/source/}"
                              ;;
                            /nix/store/src/*)
                              resolved="$PWD/src/''${target#/nix/store/src/}"
                              ;;
                          esac
                        fi
                        if [ -e "$resolved" ]; then
                          rm "$link"
                          cp "$resolved" "$link"
                        fi
                      done < <(find "$out/include" -type l -print0)
                      for spec in \
                        "utils/probes.h:src/backend/utils/probes.h" \
                        "utils/errcodes.h:src/backend/utils/errcodes.h" \
                        "utils/fmgroids.h:src/backend/utils/fmgroids.h" \
                        "utils/fmgrprotos.h:src/backend/utils/fmgrprotos.h" \
                        "nodes/nodetags.h:src/backend/nodes/nodetags.h" \
                        "pg_config_os.h:src/include/port/linux.h"; do
                        rel="''${spec%%:*}"
                        src="''${spec##*:}"
                        if [ -e "$out/include/$rel" ] || [ -L "$out/include/$rel" ]; then
                          rm -f "$out/include/$rel"
                        fi
                        if [ -e "$PWD/$src" ]; then
                          mkdir -p "$(dirname "$out/include/$rel")"
                          cp "$PWD/$src" "$out/include/$rel"
                        fi
                      done
                    fi
                  '';
                  outputChecks = { };
                  separateDebugInfo = false;
                  nativeBuildInputs = (old.nativeBuildInputs or [ ])
                    ++ [ prev.buildPackages.gcc prev.buildPackages.binutils ];
                  preConfigure = (old.preConfigure or "") + ''
                    export PATH="${prev.buildPackages.gcc}/bin:${prev.buildPackages.binutils}/bin:$PATH"
                    export CC="${prev.buildPackages.gcc}/bin/${final.stdenv.cc.targetPrefix}gcc"
                    export CXX="${prev.buildPackages.gcc}/bin/${final.stdenv.cc.targetPrefix}g++"
                    export AR="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ar"
                    export RANLIB="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ranlib"
                    export LD="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ld"
                    export READELF="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}readelf"
                    export STRIP="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}strip"
                    export OBJCOPY="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}objcopy"
                    export CFLAGS="$(echo "$CFLAGS" | sed -e 's/-flto//g')"
                    export NIX_CFLAGS_COMPILE="$(echo "$NIX_CFLAGS_COMPILE" | sed -e 's/-flto//g')"
                  '';
                })
              else
                prev.postgresql_17;

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
              compiler-nix-name = "ghc9122";

              # Workaround for: https://github.com/input-output-hk/haskell.nix/issues/2423
              modules = [
                {
                  packages.directory.flags.os-string = true;
                  packages.unix.flags.os-string = true;
                }
                {
                  packages.postgresql-libpq-configure.components.library.preConfigure =
                    ''
                      cat > pg_config <<'EOF'
                      #!${pkgs.runtimeShell}
                      case "$1" in
                        --includedir) echo "${pkgs.pkgsCross.aarch64-multiplatform.postgresql}/include" ;;
                        --libdir) echo "${pkgs.pkgsCross.aarch64-multiplatform.postgresql}/lib" ;;
                        --cppflags) echo "-I${pkgs.pkgsCross.aarch64-multiplatform.postgresql}/include" ;;
                        --ldflags) echo "-L${pkgs.pkgsCross.aarch64-multiplatform.postgresql}/lib" ;;
                        --libs) echo "-lpq" ;;
                        --version) echo "17.7" ;;
                        *) exit 0 ;;
                      esac
                      EOF
                      chmod +x pg_config
                      export PG_CONFIG="$PWD/pg_config"
                    '';
                }
                {
                  packages.persistent-postgresql.components.library.preConfigure =
                    ''
                      if [ -n "$LD_LIBRARY_PATH" ]; then
                        export LD_LIBRARY_PATH="${pkgs.postgresql}/lib:$LD_LIBRARY_PATH"
                      else
                        export LD_LIBRARY_PATH="${pkgs.postgresql}/lib"
                      fi
                    '';
                }
                (let
                  iserv =
                    "${pkgs.buildPackages.haskell.compiler.ghc9122}/lib/ghc-9.12.2/bin/ghc-iserv-dyn";
                  thOpts = [ "-fexternal-interpreter" "-pgmi" iserv ];
                in {
                  # Force a build-platform iserv to avoid TH execution under qemu.
                  packages.yesod-newsfeed.components.library.ghcOptions =
                    thOpts;
                  packages.yesod-form.components.library.ghcOptions = thOpts;
                  packages.yesod-core.components.library.ghcOptions = thOpts;
                  packages.shakespeare.components.library.ghcOptions = thOpts;
                  packages.shakespeare-sass.components.library.ghcOptions =
                    thOpts;
                  packages.twenty48.components.library.ghcOptions = thOpts;
                })
                {
                  packages.hlibsass.flags.externallibsass = true;
                  packages.hlibsass.configureFlags = [
                    "--extra-include-dirs=${pkgs.pkgsCross.aarch64-multiplatform.libsass}/include"
                    "--extra-lib-dirs=${pkgs.pkgsCross.aarch64-multiplatform.libsass}/lib"
                  ];
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
                    preConfigure = ''
                      export CFLAGS="''${CFLAGS:-} -fPIC"
                      export CXXFLAGS="''${CXXFLAGS:-} -fPIC"
                      export NIX_CFLAGS_COMPILE="''${NIX_CFLAGS_COMPILE:-} -fPIC -I${pkgs.pkgsCross.aarch64-multiplatform.libsass}/include"
                      export NIX_CXXFLAGS_COMPILE="''${NIX_CXXFLAGS_COMPILE:-} -fPIC -I${pkgs.pkgsCross.aarch64-multiplatform.libsass}/include"
                      export NIX_LDFLAGS="''${NIX_LDFLAGS:-} -L${pkgs.pkgsCross.aarch64-multiplatform.libsass}/lib"
                      export C_INCLUDE_PATH="${pkgs.pkgsCross.aarch64-multiplatform.libsass}/include''${C_INCLUDE_PATH:+:}''${C_INCLUDE_PATH:-}"
                      export CPLUS_INCLUDE_PATH="${pkgs.pkgsCross.aarch64-multiplatform.libsass}/include''${CPLUS_INCLUDE_PATH:+:}''${CPLUS_INCLUDE_PATH:-}"
                      export LIBRARY_PATH="${pkgs.pkgsCross.aarch64-multiplatform.libsass}/lib''${LIBRARY_PATH:+:}''${LIBRARY_PATH:-}"
                    '';
                    postInstall = ''
                      while IFS= read -r -d "" f; do
                        if [ "$(head -c 4 "$f")" = "$(printf '\x7fELF')" ]; then
                          patchelf --remove-rpath "$f"
                        fi
                      done < <(find "$out" -type f -name 'libHShlibsass*.so*' -print0)
                    '';
                  };
                }
                {
                  packages.twenty48.components.exes.twenty48 = {
                    postInstall = ''
                      mkdir -p "$out/share/twenty48"
                      cp -R "${./static}" "$out/share/twenty48/static"
                      mv "$out/bin/twenty48" "$out/bin/twenty48.real"
                      cat > "$out/bin/twenty48" <<'EOF'
                      #!${pkgs.runtimeShell}
                      self_dir="$(cd "$(dirname "$0")" && pwd)"
                      export STATIC_DIR="$self_dir/../share/twenty48/static"
                      exec "$self_dir/twenty48.real" "$@"
                      EOF
                      chmod +x "$out/bin/twenty48"
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

          # Build a dynamically linked binary for 64bit ARM
          twenty48-rpi =
            (pkgs.pkgsCross.aarch64-multiplatform.twenty48Project.flake
              { }).packages."twenty48:exe:twenty48";

        };
      });
}
