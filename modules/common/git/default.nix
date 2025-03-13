{ config
, lib
, pkgs
, ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  programs.git = {
    enable = true;

    extraConfig = {
      branch.sort = "-committerdate";
      color.ui = "auto";
      column.ui = "auto";
      commit.verbose = true;
      help.autocorrect = "prompt";
      init.defaultBranch = "main";
      pull.rebase = true;
      tag.sort = "version:refname";

      fetch = {
        prune = true;
        pruneTags = true;
      };

      push = {
        default = "simple";
        autoSetupRemote = true;
        followTags = true;
      };

      rebase = {
        autoSquash = true;
        autoStash = true;
        updateRefs = true;
      };
    };

    # n.b. per-user configuration goes with the user...
  };
}
