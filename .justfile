# direnv allow
@allow:
  direnv allow

# direnv reload
@reload:
  direnv reload

#  cli
@cli *OPTS:
  cabal run . -- {{ OPTS }} 2>&1 || true

#  cli
@server:
  nix run ".#raffleize:exe:server"
@tui:
  nix run ".#raffleize:exe:tui"
@psgen:
  nix run ".#raffleize:exe:psgen"
@test:
  nix run ".#raffleize:test:tests"



# cabal repl
@repl:
  cabal repl


# create HLS symlink
# Find haskell-language-server and link it into .vscode
@link-hls:
  hls_bin=$(which haskell-language-server); \
  if [ -n "$hls_bin" ]; then \
  mkdir -p .vscode; \
  ln -sf "$hls_bin" .vscode/haskell-language-server.link; \
  echo "Linked $hls_bin to .vscode/haskell-language-server.link"; \
  else \
  echo "haskell-language-server not found!"; \
  fi


# clean nix store
@gc:
  nix-collect-garbage -d

