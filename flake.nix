{
  description = "Pellucid: a Pixelfed client in PureScript (Halogen) with a Rust (Tauri 2) shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, purescript-overlay, rust-overlay }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ purescript-overlay.overlays.default rust-overlay.overlays.default ];
        config = {
          # The Android SDK and NDK are unfree; only the `android` shell uses them.
          allowUnfree = true;
          android_sdk.accept_license = true;
        };
      };

      purescriptTools = pkgs: [
        pkgs.purs
        pkgs.spago
        pkgs.purs-tidy
        pkgs.purs-backend-es
        pkgs.purescript-language-server
        pkgs.esbuild
        # `spago test` and `spago run` execute the compiled output with node.
        # No npm packages are used; see the README.
        pkgs.nodejs
      ];

      rustToolchain = pkgs: pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" "rust-analyzer" ];
      };

      # What Tauri 2 needs on Linux to compile and to show a WebKitGTK window.
      tauriLinuxLibs = pkgs: [
        pkgs.webkitgtk_4_1
        pkgs.gtk3
        pkgs.libsoup_3
        pkgs.glib
        pkgs.cairo
        pkgs.pango
        pkgs.gdk-pixbuf
        pkgs.atk
        pkgs.librsvg
        pkgs.libayatana-appindicator
        pkgs.openssl
        pkgs.dbus
        pkgs.xdotool
        pkgs.glib-networking
        pkgs.xdg-utils
      ];

      # WebKitGTK plays <video> and <audio> through GStreamer and finds its
      # plugins at run time on GST_PLUGIN_SYSTEM_PATH_1_0. Nothing in
      # tauriLinuxLibs puts a plugin on that path, so without this list the
      # webview has only the plugins the host session happens to export, built
      # from whatever nixpkgs the host is on.
      #   base   playbin and the audio and video converters
      #   good   the MP4 and Matroska demuxers, VP8 and VP9, the audio sinks
      #   bad    the H.264 and H.265 parsers, and the WebVTT encoder WebKit
      #          looks for every time a media element loads
      #   libav  the H.264 and AAC decoders
      gstreamerPlugins = pkgs: [
        pkgs.gst_all_1.gstreamer
        pkgs.gst_all_1.gst-plugins-base
        pkgs.gst_all_1.gst-plugins-good
        pkgs.gst_all_1.gst-plugins-bad
        pkgs.gst_all_1.gst-libav
      ];

      gstPluginPath = pkgs:
        pkgs.lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (gstreamerPlugins pkgs);

      gsettingsPath = pkgs:
        "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:"
        + "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}";
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          # Desktop development: `spago build`, `spago bundle`, `cargo tauri dev`.
          default = pkgs.mkShell {
            packages = purescriptTools pkgs ++ [
              (rustToolchain pkgs)
              pkgs.cargo-tauri
              pkgs.pkg-config
              pkgs.gobject-introspection
            ] ++ tauriLinuxLibs pkgs ++ gstreamerPlugins pkgs;

            shellHook = ''
              export XDG_DATA_DIRS="${gsettingsPath pkgs}:''${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
              # The webview's media plugins come from this flake and from nowhere
              # else, so they match the GStreamer that WebKitGTK is linked to.
              export GST_PLUGIN_SYSTEM_PATH_1_0="${gstPluginPath pkgs}"
              # WebKitGTK loads TLS support from glib-networking at runtime; without
              # this the webview cannot fetch https images.
              export GIO_MODULE_DIR="${pkgs.glib-networking}/lib/gio/modules/"
              # Avoids a blank WebKitGTK window on some GPU drivers.
              export WEBKIT_DISABLE_DMABUF_RENDERER=1
              echo "pellucid dev shell: purs $(purs --version), spago $(spago --version), $(cargo --version)"
            '';
          };

          # Android builds. Not yet exercised; expect to adjust SDK versions.
          # Requires `cargo tauri android init` once, then `cargo tauri android dev`.
          android =
            let
              androidComposition = pkgs.androidenv.composeAndroidPackages {
                platformVersions = [ "34" ];
                buildToolsVersions = [ "34.0.0" ];
                includeNDK = true;
                ndkVersions = [ "26.1.10909125" ];
                includeEmulator = false;
                includeSystemImages = false;
              };
              androidRust = pkgs.rust-bin.stable.latest.default.override {
                extensions = [ "rust-src" "rust-analyzer" ];
                targets = [
                  "aarch64-linux-android"
                  "armv7-linux-androideabi"
                  "x86_64-linux-android"
                  "i686-linux-android"
                ];
              };
            in
            pkgs.mkShell {
              packages = purescriptTools pkgs ++ [
                androidRust
                pkgs.cargo-tauri
                pkgs.pkg-config
                pkgs.jdk17
                androidComposition.androidsdk
              ] ++ tauriLinuxLibs pkgs;

              shellHook = ''
                export ANDROID_HOME="${androidComposition.androidsdk}/libexec/android-sdk"
                export ANDROID_SDK_ROOT="$ANDROID_HOME"
                export NDK_HOME="$ANDROID_HOME/ndk/26.1.10909125"
                export JAVA_HOME="${pkgs.jdk17}"
                export XDG_DATA_DIRS="${gsettingsPath pkgs}:''${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
                export GIO_MODULE_DIR="${pkgs.glib-networking}/lib/gio/modules/"
                echo "pellucid android shell: ANDROID_HOME=$ANDROID_HOME"
              '';
            };
        });

      formatter = forAllSystems (system: (pkgsFor system).nixpkgs-fmt);
    };
}