{ config
, lib
, pkgs
, ...
}:
{
  stylix.enable = true;
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/primer-dark-dimmed.yaml";
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/nord-dark.yaml";

  # TODO(b7r6): get this integrated...
  # stylix.base16Scheme = ./ono-sendai.yaml;
}
