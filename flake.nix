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

            /* Dear programmer:
                When Codex wrote this code, only God and Codex knew how it worked.
                Now, only God knows it!

                Clang refused to cross-compile PostgreSQL for ARM 64.
                I sent Codex 5.2 to fix this and after _quite_ a few hours, it finally worked.

                I do not know how it works, I do not want to know how it works.
                I pray to all deities and then some I will never have to touch this again.

                I pasted below the error message that sent me down this path of madness:

                ```
                > checking for aarch64-unknown-linux-gnu-gcc... aarch64-unknown-linux-gnu-clang
                > checking whether the C compiler works... no
                > configure: error: in `/build/source':
                > configure: error: C compiler cannot create executables
                > See `config.log' for more details
                ```

                ```
                configure:4021: aarch64-unknown-linux-gnu-clang -V >&5
                clang: error: unsupported option '-V -U_FORTIFY_SOURCE'
                clang: error: no input files
                configure:4032: $? = 1
                configure:4021: aarch64-unknown-linux-gnu-clang -qversion >&5
                clang: error: unknown argument '-qversion'; did you mean '--version'?
                clang: error: no input files
                configure:4032: $? = 1
                configure:4052: checking whether the C compiler works
                configure:4074: aarch64-unknown-linux-gnu-clang -fdata-sections -ffunction-sections -flto   conftest.c  >&5
                clang: error: unable to execute command: posix_spawn failed: Exec format error
                clang: error: linker command failed with exit code 1 (use -v to see invocation)
                configure:4078: $? = 1
                configure:4116: result: no
                ```
            */

            /* PostgreSQL cross build note:
               The clang wrapper failed for aarch64 with exec-format errors
               during configure. We force GCC and strip -flto so configure can
               compile test binaries. We also keep PostgreSQL as a single
               output and regenerate pg_config.env for downstream libpq users.
            */
            postgresql =
              if final.stdenv.buildPlatform != final.stdenv.hostPlatform then
                prev.postgresql.overrideAttrs (old: {
                  outputs = [ "out" ];
                  prePatch = (old.prePatch or "") + ''
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
                  '';
                  outputChecks = { };
                  separateDebugInfo = false;
                  preConfigure = (old.preConfigure or "") + ''
                    export CC="${prev.buildPackages.gcc}/bin/${final.stdenv.cc.targetPrefix}gcc"
                    export CXX="${prev.buildPackages.gcc}/bin/${final.stdenv.cc.targetPrefix}g++"
                    export AR="${prev.buildPackages.binutils}/bin/${final.stdenv.cc.targetPrefix}ar"
                    export CFLAGS="$(echo "$CFLAGS" | sed -e 's/-flto//g')"
                  '';
                })
              else
                prev.postgresql;

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

              # Had to upgrade to GHC 9.12.2 to get `yesod-newsfeed` to compile, I was having Template Haskell related issues.
              compiler-nix-name = "ghc9122";

              modules = [
                {
                  # Workaround for: https://github.com/input-output-hk/haskell.nix/issues/2423
                  packages.directory.flags.os-string = true;
                  packages.unix.flags.os-string = true;
                }
                {
                  # > configure: error: Library requirements (PostgreSQL) not met.
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

                  /* Cross builds: point hlibsass at the cross libsass headers
                      and libs to avoid libsass detection failures.

                      Example failure:

                      > Building library for twenty48-0.0.0...
                      > [ 1 of 28] Compiling Model            ( src/Model.hs, dist/build/Model.o )
                      > ---> Starting iserv-proxy-interpreter on port 7394
                      > ---| iserv-proxy-interpreter should have started on 7394
                      > Listening on port 7394
                      > iserv-proxy-interpreter: internal error: 0x0 address for _ZGVZN4utf815replace_invalidIPKcSt20back_insert_iteratorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEEET0_T_SC_SB_E18replacement_marker + 0 of type 311 in tmp/nix/store/ma63g9gw8yydi8n9hv31lrqhr6aybk6a-hlibsass-lib-hlibsass-aarch64-unknown-linux-gnu-0.1.10.3/lib/aarch64-linux-ghc-9.12.2-inplace/hlibsass-0.1.10.3-BdQqbJmmOCNA5pDyG35TDD/libsass.a(#53:sass_context.o) for relocation 154 in section 52 of kind: 0
                      >
                      >     (GHC version 9.12.2 for aarch64_unknown_linux)
                      >     Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
                      > qemu: uncaught target signal 6 (Aborted) - core dumped
                      > iserv-proxy: Uncaught exception ghc-internal:GHC.Internal.IO.Exception.IOException:
                      >
                      > {handle: <socket: 15>}: GHCi.Message.remoteCall: end of file
                      >
                      > HasCallStack backtrace:
                      >   collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:169:13 in ghc-internal:GHC.Internal.Exception
                      >   toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:89:42 in ghc-internal:GHC.Internal.Exception
                      >   throw, called at libraries/ghci/GHCi/Message.hs:673:16 in ghci-9.12.2-inplace:GHCi.Message
                      >
                      > <no location info>: error: External interpreter terminated (1)
                      >
                      > [ 2 of 28] Compiling Paths_twenty48   ( dist/build/autogen/Paths_twenty48.hs, dist/build/Paths_twenty48.o )
                      > [ 3 of 28] Compiling Settings         ( src/Settings.hs, dist/build/Settings.o )
                      > <no location info>: error: External interpreter terminated (1)
                  */
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

          # Build a dynamically linked binary for 64bit ARM
          twenty48-rpi =
            (pkgs.pkgsCross.aarch64-multiplatform.twenty48Project.flake
              { }).packages."twenty48:exe:twenty48";

        };
      });
}
