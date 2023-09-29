let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).

      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
    '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).

      You may need to augment the system package mapping in haskell.nix so that it can be found.
    '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).

      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
    '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
    '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).

      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
    '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).

      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.

      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
    '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
{
  flags = {
    with_xft = false;
    with_utf8 = true;
    with_inotify = false;
    with_iwlib = false;
    with_mpd = false;
    all_extensions = false;
    with_alsa = false;
    with_datezone = false;
    with_mpris = false;
    with_dbus = false;
    with_xpm = false;
    with_threaded = false;
    with_rtsopts = true;
    with_uvmeter = false;
    with_weather = true;
  };
  package = {
    specVersion = "1.8";
    identifier = {
      name = "xmobar";
      version = "0.30";
    };
    license = "BSD-3-Clause";
    copyright = "";
    maintainer = "Jose A. Ortega Ruiz <jao@gnu.org>";
    author = "Andrea Rossato and Jose A. Ortega Ruiz";
    homepage = "http://xmobar.org";
    url = "";
    synopsis = "A Minimalistic Text Based Status Bar";
    description = ''
      Xmobar is a minimalistic text based status bar.

      Inspired by the Ion3 status bar, it supports similar
      features, like dynamic color management, output templates,
      and extensibility through plugins.'';
    buildType = "Simple";
    isLocal = true;
    detailLevel = "FullDetails";
    licenseFiles = [ "license" ];
    dataDir = "";
    dataFiles = [ ];
    extraSrcFiles = [
      "readme.md"
      "changelog.md"
      "examples/padding-icon.sh"
      "examples/xmobar.config"
      "examples/xmobar.hs"
      "examples/xmonadpropwrite.hs"
    ];
    extraTmpFiles = [ ];
    extraDocFiles = [ ];
  };
  components = {
    "library" = {
      depends = ((((((((((([
        (hsPkgs."base" or (buildDepError "base"))
        (hsPkgs."containers" or (buildDepError "containers"))
        (hsPkgs."regex-compat" or (buildDepError "regex-compat"))
        (hsPkgs."process" or (buildDepError "process"))
        (hsPkgs."old-locale" or (buildDepError "old-locale"))
        (hsPkgs."bytestring" or (buildDepError "bytestring"))
        (hsPkgs."directory" or (buildDepError "directory"))
        (hsPkgs."unix" or (buildDepError "unix"))
        (hsPkgs."time" or (buildDepError "time"))
        (hsPkgs."filepath" or (buildDepError "filepath"))
        (hsPkgs."transformers" or (buildDepError "transformers"))
        (hsPkgs."X11" or (buildDepError "X11"))
        (hsPkgs."mtl" or (buildDepError "mtl"))
        (hsPkgs."parsec" or (buildDepError "parsec"))
        (hsPkgs."parsec-numbers" or (buildDepError "parsec-numbers"))
        (hsPkgs."stm" or (buildDepError "stm"))
        (hsPkgs."extensible-exceptions" or (buildDepError
          "extensible-exceptions"))
        (hsPkgs."async" or (buildDepError "async"))
      ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0.2")
        (hsPkgs."unsupported-ghc-version" or (buildDepError
          "unsupported-ghc-version")))
        ++ (pkgs.lib).optionals (flags.with_xft || flags.all_extensions) [
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."X11-xft" or (buildDepError "X11-xft"))
        ]) ++ (pkgs.lib).optional (flags.with_utf8 || flags.all_extensions)
        (hsPkgs."utf8-string" or (buildDepError "utf8-string")))
        ++ (pkgs.lib).optional (flags.with_inotify || flags.all_extensions)
        (hsPkgs."hinotify" or (buildDepError "hinotify")))
        ++ (pkgs.lib).optional (flags.with_iwlib || flags.all_extensions)
        (hsPkgs."iwlib" or (buildDepError "iwlib")))
        ++ (pkgs.lib).optional (flags.with_mpd || flags.all_extensions)
        (hsPkgs."libmpd" or (buildDepError "libmpd")))
        ++ (pkgs.lib).optionals (flags.with_alsa || flags.all_extensions) [
          (hsPkgs."alsa-mixer" or (buildDepError "alsa-mixer"))
          (hsPkgs."alsa-core" or (buildDepError "alsa-core"))
          (hsPkgs."process" or (buildDepError "process"))
        ])
        ++ (pkgs.lib).optionals (flags.with_datezone || flags.all_extensions) [
          (hsPkgs."timezone-olson" or (buildDepError "timezone-olson"))
          (hsPkgs."timezone-series" or (buildDepError "timezone-series"))
        ]) ++ (pkgs.lib).optional (flags.with_mpris || flags.all_extensions)
        (hsPkgs."dbus" or (buildDepError "dbus")))
        ++ (pkgs.lib).optional (flags.with_dbus || flags.all_extensions)
        (hsPkgs."dbus" or (buildDepError "dbus")))
        ++ (pkgs.lib).optionals (flags.with_weather || flags.all_extensions) [
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
        ]) ++ (pkgs.lib).optionals (flags.with_uvmeter) [
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
        ];
      libs = ([
        (pkgs."Xrandr" or (sysDepError "Xrandr"))
        (pkgs."Xrender" or (sysDepError "Xrender"))
      ] ++ (pkgs.lib).optional (flags.with_iwlib || flags.all_extensions)
        (pkgs."iw" or (sysDepError "iw")))
        ++ (pkgs.lib).optional (flags.with_xpm || flags.all_extensions)
        (pkgs."Xpm" or (sysDepError "Xpm"));
      buildable = true;
      modules = (((((((((([
        "Paths_xmobar"
        "Xmobar/Config/Types"
        "Xmobar/Config/Parse"
        "Xmobar/Run/Types"
        "Xmobar/Run/Template"
        "Xmobar/Run/Exec"
        "Xmobar/Run/Command"
        "Xmobar/Run/Runnable"
        "Xmobar/App/EventLoop"
        "Xmobar/App/Config"
        "Xmobar/App/Main"
        "Xmobar/App/Opts"
        "Xmobar/App/Compile"
        "Xmobar/System/Utils"
        "Xmobar/System/StatFS"
        "Xmobar/System/Environment"
        "Xmobar/System/Localize"
        "Xmobar/System/Signal"
        "Xmobar/System/Kbd"
        "Xmobar/X11/Actions"
        "Xmobar/X11/Events"
        "Xmobar/X11/Parsers"
        "Xmobar/X11/Types"
        "Xmobar/X11/Text"
        "Xmobar/X11/Bitmap"
        "Xmobar/X11/ColorCache"
        "Xmobar/X11/Window"
        "Xmobar/X11/Draw"
        "Xmobar/Plugins/BufferedPipeReader"
        "Xmobar/Plugins/CommandReader"
        "Xmobar/Plugins/Date"
        "Xmobar/Plugins/EWMH"
        "Xmobar/Plugins/PipeReader"
        "Xmobar/Plugins/MarqueePipeReader"
        "Xmobar/Plugins/StdinReader"
        "Xmobar/Plugins/XMonadLog"
        "Xmobar/Plugins/Kbd"
        "Xmobar/Plugins/Locks"
        "Xmobar/Plugins/Monitors"
        "Xmobar/Plugins/Monitors/Batt"
        "Xmobar/Plugins/Monitors/Common"
        "Xmobar/Plugins/Monitors/Common/Types"
        "Xmobar/Plugins/Monitors/Common/Run"
        "Xmobar/Plugins/Monitors/Common/Output"
        "Xmobar/Plugins/Monitors/Common/Parsers"
        "Xmobar/Plugins/Monitors/Common/Files"
        "Xmobar/Plugins/Monitors/CoreTemp"
        "Xmobar/Plugins/Monitors/CpuFreq"
        "Xmobar/Plugins/Monitors/Cpu"
        "Xmobar/Plugins/Monitors/Disk"
        "Xmobar/Plugins/Monitors/Mem"
        "Xmobar/Plugins/Monitors/MultiCoreTemp"
        "Xmobar/Plugins/Monitors/MultiCpu"
        "Xmobar/Plugins/Monitors/Net"
        "Xmobar/Plugins/Monitors/Swap"
        "Xmobar/Plugins/Monitors/Thermal"
        "Xmobar/Plugins/Monitors/ThermalZone"
        "Xmobar/Plugins/Monitors/Top"
        "Xmobar/Plugins/Monitors/Uptime"
        "Xmobar/Plugins/Monitors/Bright"
        "Xmobar/Plugins/Monitors/CatInt"
        "Xmobar"
      ] ++ (pkgs.lib).optional (flags.with_xft || flags.all_extensions)
        "Xmobar/X11/MinXft")
        ++ (pkgs.lib).optionals (flags.with_inotify || flags.all_extensions) [
          "Xmobar/Plugins/Mail"
          "Xmobar/Plugins/MBox"
        ]) ++ (pkgs.lib).optional (flags.with_iwlib || flags.all_extensions)
        "Xmobar/Plugins/Monitors/Wireless")
        ++ (pkgs.lib).optional (flags.with_mpd || flags.all_extensions)
        "Xmobar/Plugins/Monitors/MPD")
        ++ (pkgs.lib).optionals (flags.with_alsa || flags.all_extensions) [
          "Xmobar/Plugins/Monitors/Volume"
          "Xmobar/Plugins/Monitors/Alsa"
        ]) ++ (pkgs.lib).optional (flags.with_datezone || flags.all_extensions)
        "Xmobar/Plugins/DateZone")
        ++ (pkgs.lib).optional (flags.with_mpris || flags.all_extensions)
        "Xmobar/Plugins/Monitors/Mpris")
        ++ (pkgs.lib).optional (flags.with_dbus || flags.all_extensions)
        "Xmobar/System/DBus")
        ++ (pkgs.lib).optional (flags.with_xpm || flags.all_extensions)
        "Xmobar/X11/XPMFile")
        ++ (pkgs.lib).optional (flags.with_weather || flags.all_extensions)
        "Xmobar/Plugins/Monitors/Weather")
        ++ (pkgs.lib).optional (flags.with_uvmeter)
        "Xmobar/Plugins/Monitors/UVMeter";
      hsSourceDirs = [ "src" ];
    };
    exes = {
      "xmobar" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."X11" or (buildDepError "X11"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."unix" or (buildDepError "unix"))
          (hsPkgs."parsec" or (buildDepError "parsec"))
          (hsPkgs."xmobar" or (buildDepError "xmobar"))
        ];
        buildable = true;
        hsSourceDirs = [ "app" ];
        mainPath =
          ([ "Main.hs" ] ++ (pkgs.lib).optional (flags.with_rtsopts) "")
          ++ (pkgs.lib).optional (flags.with_threaded) "";
      };
    };
    tests = {
      "XmobarTest" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."regex-compat" or (buildDepError "regex-compat"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."old-locale" or (buildDepError "old-locale"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."unix" or (buildDepError "unix"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."X11" or (buildDepError "X11"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."parsec" or (buildDepError "parsec"))
          (hsPkgs."parsec-numbers" or (buildDepError "parsec-numbers"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."temporary" or (buildDepError "temporary"))
          (hsPkgs."hspec" or (buildDepError "hspec"))
          (hsPkgs."xmobar" or (buildDepError "xmobar"))
        ] ++ (pkgs.lib).optionals (flags.with_alsa || flags.all_extensions) [
          (hsPkgs."alsa-mixer" or (buildDepError "alsa-mixer"))
          (hsPkgs."alsa-core" or (buildDepError "alsa-core"))
          (hsPkgs."process" or (buildDepError "process"))
        ];
        buildable = true;
        modules = [
          "Xmobar/Plugins/Monitors/CommonSpec"
          "Xmobar/Plugins/Monitors/Common"
          "Xmobar/Plugins/Monitors/Common/Parsers"
          "Xmobar/Plugins/Monitors/Common/Run"
          "Xmobar/Plugins/Monitors/Common/Types"
          "Xmobar/Plugins/Monitors/Common/Output"
          "Xmobar/Plugins/Monitors/Common/Files"
          "Xmobar/Run/Exec"
          "Xmobar/System/Signal"
        ] ++ (pkgs.lib).optionals (flags.with_alsa || flags.all_extensions) [
          "Xmobar/Plugins/Monitors/Volume"
          "Xmobar/Plugins/Monitors/Alsa"
          "Xmobar/Plugins/Monitors/AlsaSpec"
        ];
        hsSourceDirs = [ "src" "test" ];
        mainPath = [ "Spec.hs" ];
      };
    };
  };
} // rec {
  src = (pkgs.lib).mkDefault ../.;
}
