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


hls-bin := `which haskell-language-server`

# create HLS symlink
@link-hls:
  if [ -n "{{ hls-bin }}" ]; then \
    ln -s -f "{{ hls-bin }}" .vscode/haskell-language-server.link; \
  else echo "haskell-language-server not found!"; \
  fi


# clean nix store
@gc:
  nix-collect-garbage -d

