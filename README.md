# Install dependencies

```shell
go install golang.org/x/tools/gopls@latest
go install github.com/rogpeppe/godef@latest
go install github.com/go-delve/delve/cmd/dlv@latest
curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
apt install clangd

# Nix profile
nix profile install github:nixos/nixpkgs#nixd nixpkgs#gopls nixpkgs#godef nixpkgs#delve nixpkgs#source-code-pro nixpkgs#clang nixpkgs#asm-lsp

# Haskell
ghcup install hls

haskell-language-server-

# ansible
npm i -g @ansible/ansible-language-server
```

In emacs remember to run
```
M-x all-the-icons-install-fonts
```
