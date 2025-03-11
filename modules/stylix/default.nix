{ config
, lib
, pkgs
, ...
}:
{
  stylix.enable = true;
  stylix.base16Scheme = ./ono-sendai.yaml;
}
