{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Define colors (based on Ono-Sendai theme)
  colors = {
    # Base colors
    base00 = "101216"; # Dark background
    base01 = "161B22"; # Lighter background (highlights)
    base02 = "1B1F23"; # Selection background
    base03 = "5C6370"; # Comments, invisibles
    base04 = "676E7D"; # Dark foreground
    base05 = "bababa"; # Default foreground
    base06 = "d3d7cf"; # Light foreground
    base07 = "ffffff"; # Light background

    # Colors
    base08 = "f78166"; # Red (variables)
    base09 = "f49b4f"; # Orange (integers, booleans)
    base0A = "FDA656"; # Yellow (classes, search)
    base0B = "8ddb8c"; # Green (strings)
    base0C = "96cffe"; # Cyan (escape chars, regex)
    base0D = "539bf5"; # Blue (functions, methods)
    base0E = "d5b7f4"; # Magenta (keywords)
    base0F = "AB5DFF"; # Purple (deprecated)
  };

  # Create the Stylix scheme from our colors
  scheme = {
    slug = "ono-sendai-hyper-modern";
    name = "Ono-Sendai Hyper Modern";
    author = "b7r6";
    inherit colors;
  };
in
{
  # Super minimal version to fix build errors
  stylix = {
    # Use our custom color scheme
    base16Scheme = scheme;

    # Set default fonts 
    fonts = {
      monospace = {
        name = "JetBrains Mono";
        package = pkgs.jetbrains-mono;
      };
      sansSerif = {
        name = "Inter";
        package = pkgs.inter;
      };
      serif = {
        name = "Noto Serif";
        package = pkgs.noto-fonts;
      };
    };

    # Disable anything complex until we can determine what's supported
    polarity = "dark";
  };
}