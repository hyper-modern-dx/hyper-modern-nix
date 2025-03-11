{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  # Your existing configuration...

  programs.atuin = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      # Basic settings
      update_check = false;
      dialect = "us";

      # Theme configuration - correctly structured based on Atuin's expectations
      # Atuin requires this to be either a string or a table with one key
      style = "auto"; # Use one of: "auto", "menu", "grid"

      # Custom theme colors using the proper structure
      theme = {
        # Map semantic meanings to Base16 colors
        Base = colors.base05;
        Title = colors.base0D;
        Important = colors.base0E;
        Annotation = colors.base03;
        Guidance = colors.base0C;
        AlertInfo = colors.base0B;
        AlertWarn = colors.base0A;
        AlertError = colors.base08;
      };
    };
  };
}
