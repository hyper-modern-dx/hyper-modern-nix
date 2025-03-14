{ config
, lib
, pkgs
, ...
}:
let
  berkeley-mono = pkgs.stdenv.mkDerivation {
      name = "berkeley-mono-font";
      src = ./.; # Directory containing your font files
      installPhase = ''
      mkdir -p $out/share/fonts/opentype
      cp ${./berkeley-mono-semi-bold.otf} $out/share/fonts/opentype/
    '';
  };
  in
{
  stylix.enable = true;
  stylix.base16Scheme = ./ono-sendai.yaml;
  stylix = {
    fonts = {
      serif = {
              package = berkeley-mono;
              name = "Berkeley Mono";
      };
      sansSerif = {
              package = berkeley-mono;
              name = "Berkeley Mono";
      };
      monospace = {
              package = berkeley-mono;
              name = "Berkeley Mono";
      };
      emoji = {
              package = pkgs.noto-fonts-emoji;
              name = "Noto Color Emoji";
      };
      sizes = {
              applications = 12;
              desktop = 12;
              terminal = 12;
      };
    };
  };
}
