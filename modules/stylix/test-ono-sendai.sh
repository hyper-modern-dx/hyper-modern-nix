#!/usr/bin/env bash
# Base16 Ono-Sendai Hyper Modern for terminal

export BASE16_THEME="ono-sendai-hyper-modern"
export BASE00="#101216"
export BASE01="#171C22"
export BASE02="#171C22"
export BASE03="#d5b7f4"
export BASE04="#262C32"
export BASE05="#96cffe"
export BASE06="#E5EBF2"
export BASE07="#F0F6FC"
export BASE08="#f78166"
export BASE09="#f49b4f"
export BASE0A="#f49b4f"
export BASE0B="#539bf5"
export BASE0C="#d5b7f4"
export BASE0D="#539bf5"
export BASE0E="#96cffe"
export BASE0F="#8ddb8c"

# Optional: if using base16-shell, source the helper script.
[ -n "$PS1" ] &&
  [ -s "$HOME/.config/base16-shell/profile_helper.sh" ] &&
  eval "$("$HOME/.config/base16-shell/profile_helper.sh")"

# To preview the colors, run a snippet that prints the swatches:
for i in {0..15}; do
  color_var=$(printf "BASE%02X" "$i")
  color=$(eval echo \$$color_var)
  # Convert hex to R, G, B (strip the # first)
  r=$(printf "%d" 0x${color:1:2})
  g=$(printf "%d" 0x${color:3:2})
  b=$(printf "%d" 0x${color:5:2})
  printf "\e[48;2;%d;%d;%dm  \e[0m" "$r" "$g" "$b"
done
printf "\n"
